// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Oklab colorspace conversions.
//!
//! Implements Oklab as defined at: <https://bottosson.github.io/posts/oklab/>

#![allow(clippy::excessive_precision)]

use std::fmt::Debug;

use crate::simd::MemsetSafe;

/// A sRGB color with straight (= not premultiplied) alpha.
#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct StraightRgba(u32);

impl StraightRgba {
    #[inline]
    pub const fn zero() -> Self {
        StraightRgba(0)
    }

    #[inline]
    pub const fn from_le(color: u32) -> Self {
        StraightRgba(u32::from_le(color))
    }

    #[inline]
    pub const fn from_be(color: u32) -> Self {
        StraightRgba(u32::from_be(color))
    }

    #[inline]
    pub const fn to_ne(self) -> u32 {
        self.0
    }

    #[inline]
    pub const fn to_le(self) -> u32 {
        self.0.to_le()
    }

    #[inline]
    pub const fn to_be(self) -> u32 {
        self.0.to_be()
    }

    #[inline]
    pub const fn red(self) -> u32 {
        self.0 & 0xff
    }

    #[inline]
    pub const fn green(self) -> u32 {
        (self.0 >> 8) & 0xff
    }

    #[inline]
    pub const fn blue(self) -> u32 {
        (self.0 >> 16) & 0xff
    }

    #[inline]
    pub const fn alpha(self) -> u32 {
        self.0 >> 24
    }

    pub fn oklab_blend(self, top: StraightRgba) -> StraightRgba {
        let bottom = self.as_oklab();
        let top = top.as_oklab();
        let result = bottom.blend(&top);
        result.as_rgba()
    }

    pub fn as_oklab(self) -> Oklab {
        let r = srgb_to_linear(self.red());
        let g = srgb_to_linear(self.green());
        let b = srgb_to_linear(self.blue());
        let alpha = self.alpha() as f32 * (1.0 / 255.0);

        let l = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b;
        let m = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b;
        let s = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b;

        let l_ = cbrtf_est(l);
        let m_ = cbrtf_est(m);
        let s_ = cbrtf_est(s);

        let l = 0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_;
        let a = 1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_;
        let b = 0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_;

        Oklab([l, a, b, alpha])
    }
}

impl Debug for StraightRgba {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:08x}", self.0.to_be()) // Display as a hex color
    }
}

unsafe impl MemsetSafe for StraightRgba {}

/// An Oklab color with alpha. By convention, it uses straight alpha.
#[derive(Clone, Copy)]
pub struct Oklab([f32; 4]);

impl Oklab {
    #[inline]
    pub const fn lightness(self) -> f32 {
        self.0[0]
    }

    #[inline]
    pub const fn a(self) -> f32 {
        self.0[1]
    }

    #[inline]
    pub const fn b(self) -> f32 {
        self.0[2]
    }

    #[inline]
    pub const fn alpha(self) -> f32 {
        self.0[3]
    }

    pub fn as_rgba(&self) -> StraightRgba {
        let l_ = self.lightness() + 0.3963377774 * self.a() + 0.2158037573 * self.b();
        let m_ = self.lightness() - 0.1055613458 * self.a() - 0.0638541728 * self.b();
        let s_ = self.lightness() - 0.0894841775 * self.a() - 1.2914855480 * self.b();

        let l = l_ * l_ * l_;
        let m = m_ * m_ * m_;
        let s = s_ * s_ * s_;

        let r = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s;
        let g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s;
        let b = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s;

        let r = r.clamp(0.0, 1.0);
        let g = g.clamp(0.0, 1.0);
        let b = b.clamp(0.0, 1.0);
        let alpha = self.alpha().clamp(0.0, 1.0);

        let r = linear_to_srgb(r);
        let g = linear_to_srgb(g);
        let b = linear_to_srgb(b);
        let a = (alpha * 255.0) as u32;

        StraightRgba(r | (g << 8) | (b << 16) | (a << 24))
    }

    /// Porter-Duff "over" composition. It's for Lab, but it works just like with RGB.
    /// The benefit of the Oklab colorspace is its perceptual uniformity, which RGB lacks.
    /// This can be observed easily when blending red and green for instance.
    pub fn blend(&self, top: &Self) -> Self {
        let top_a = top.alpha();
        let bottom_a = self.alpha() * (1.0 - top_a);
        let l = top.lightness() * top_a + self.lightness() * bottom_a;
        let a = top.a() * top_a + self.a() * bottom_a;
        let b = top.b() * top_a + self.b() * bottom_a;
        let alpha = top_a + bottom_a;

        let inv_alpha = if alpha > 0.0 { 1.0 / alpha } else { 0.0 };
        let l = l * inv_alpha;
        let a = a * inv_alpha;
        let b = b * inv_alpha;

        Self([l, a, b, alpha])
    }
}

fn srgb_to_linear(c: u32) -> f32 {
    SRGB_TO_RGB_LUT[(c & 0xff) as usize]
}

fn linear_to_srgb(c: f32) -> u32 {
    (if c > 0.0031308 {
        255.0 * 1.055 * c.powf(1.0 / 2.4) - 255.0 * 0.055
    } else {
        255.0 * 12.92 * c
    }) as u32
}

#[inline]
fn cbrtf_est(a: f32) -> f32 {
    // http://metamerist.com/cbrt/cbrt.htm showed a great estimator for the cube root:
    //   f32_as_uint32_t / 3 + 709921077
    // It's similar to the well known "fast inverse square root" trick.
    // Lots of numbers around 709921077 perform at least equally well to 709921077,
    // and it is unknown how and why 709921077 was chosen specifically.
    let u: u32 = f32::to_bits(a); // evil f32ing point bit level hacking
    let u = u / 3 + 709921077; // what the fuck?
    let x: f32 = f32::from_bits(u);

    // One round of Newton's method. It follows the Wikipedia article at
    //   https://en.wikipedia.org/wiki/Cube_root#Numerical_methods
    // For `a`s in the range between 0 and 1, this results in a maximum error of
    // less than 6.7e-4f, which is not good, but good enough for us, because
    // we're not an image editor. The benefit is that it's really fast.
    (1.0 / 3.0) * (a / (x * x) + (x + x)) // 1st iteration
}

#[rustfmt::skip]
#[allow(clippy::excessive_precision)]
const SRGB_TO_RGB_LUT: [f32; 256] = [
    0.0000000000, 0.0003035270, 0.0006070540, 0.0009105810, 0.0012141080, 0.0015176350, 0.0018211619, 0.0021246888, 0.0024282159, 0.0027317430, 0.0030352699, 0.0033465356, 0.0036765069, 0.0040247170, 0.0043914421, 0.0047769533,
    0.0051815170, 0.0056053917, 0.0060488326, 0.0065120910, 0.0069954102, 0.0074990317, 0.0080231922, 0.0085681248, 0.0091340570, 0.0097212177, 0.0103298230, 0.0109600937, 0.0116122449, 0.0122864870, 0.0129830306, 0.0137020806,
    0.0144438436, 0.0152085144, 0.0159962922, 0.0168073755, 0.0176419523, 0.0185002182, 0.0193823613, 0.0202885624, 0.0212190095, 0.0221738834, 0.0231533647, 0.0241576303, 0.0251868572, 0.0262412224, 0.0273208916, 0.0284260381,
    0.0295568332, 0.0307134409, 0.0318960287, 0.0331047624, 0.0343398079, 0.0356013142, 0.0368894450, 0.0382043645, 0.0395462364, 0.0409151986, 0.0423114114, 0.0437350273, 0.0451862030, 0.0466650836, 0.0481718220, 0.0497065634,
    0.0512694679, 0.0528606549, 0.0544802807, 0.0561284944, 0.0578054339, 0.0595112406, 0.0612460710, 0.0630100295, 0.0648032799, 0.0666259527, 0.0684781820, 0.0703601092, 0.0722718611, 0.0742135793, 0.0761853904, 0.0781874284,
    0.0802198276, 0.0822827145, 0.0843762159, 0.0865004659, 0.0886556059, 0.0908417329, 0.0930589810, 0.0953074843, 0.0975873619, 0.0998987406, 0.1022417471, 0.1046164930, 0.1070231125, 0.1094617173, 0.1119324341, 0.1144353822,
    0.1169706732, 0.1195384338, 0.1221387982, 0.1247718409, 0.1274376959, 0.1301364899, 0.1328683347, 0.1356333494, 0.1384316236, 0.1412633061, 0.1441284865, 0.1470272839, 0.1499598026, 0.1529261619, 0.1559264660, 0.1589608639,
    0.1620294005, 0.1651322246, 0.1682693958, 0.1714410931, 0.1746473908, 0.1778884083, 0.1811642349, 0.1844749898, 0.1878207624, 0.1912016720, 0.1946178079, 0.1980693042, 0.2015562356, 0.2050787061, 0.2086368501, 0.2122307271,
    0.2158605307, 0.2195262313, 0.2232279778, 0.2269658893, 0.2307400703, 0.2345506549, 0.2383976579, 0.2422811985, 0.2462013960, 0.2501583695, 0.2541521788, 0.2581829131, 0.2622507215, 0.2663556635, 0.2704978585, 0.2746773660,
    0.2788943350, 0.2831487954, 0.2874408960, 0.2917706966, 0.2961383164, 0.3005438447, 0.3049873710, 0.3094689548, 0.3139887452, 0.3185468316, 0.3231432438, 0.3277781308, 0.3324515820, 0.3371636569, 0.3419144452, 0.3467040956,
    0.3515326977, 0.3564002514, 0.3613068759, 0.3662526906, 0.3712377846, 0.3762622178, 0.3813261092, 0.3864295185, 0.3915725648, 0.3967553079, 0.4019778669, 0.4072403014, 0.4125427008, 0.4178851545, 0.4232677519, 0.4286905527,
    0.4341537058, 0.4396572411, 0.4452012479, 0.4507858455, 0.4564110637, 0.4620770514, 0.4677838385, 0.4735315442, 0.4793202281, 0.4851499796, 0.4910208881, 0.4969330430, 0.5028865933, 0.5088814497, 0.5149177909, 0.5209956765,
    0.5271152258, 0.5332764983, 0.5394796133, 0.5457245708, 0.5520114899, 0.5583404899, 0.5647116303, 0.5711249113, 0.5775805116, 0.5840784907, 0.5906189084, 0.5972018838, 0.6038274169, 0.6104956269, 0.6172066331, 0.6239604354,
    0.6307572126, 0.6375969648, 0.6444797516, 0.6514056921, 0.6583748460, 0.6653873324, 0.6724432111, 0.6795425415, 0.6866854429, 0.6938719153, 0.7011020184, 0.7083759308, 0.7156936526, 0.7230552435, 0.7304608822, 0.7379105687,
    0.7454043627, 0.7529423237, 0.7605246305, 0.7681512833, 0.7758223414, 0.7835379243, 0.7912980318, 0.7991028428, 0.8069523573, 0.8148466945, 0.8227858543, 0.8307699561, 0.8387991190, 0.8468732834, 0.8549926877, 0.8631572723,
    0.8713672161, 0.8796223402, 0.8879231811, 0.8962693810, 0.9046613574, 0.9130986929, 0.9215820432, 0.9301108718, 0.9386858940, 0.9473065734, 0.9559735060, 0.9646862745, 0.9734454751, 0.9822505713, 0.9911022186, 1.0000000000,
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_blending() {
        let lower = StraightRgba::from_be(0x3498dbff);
        let upper = StraightRgba::from_be(0xe74c3c7f);
        let expected = StraightRgba::from_be(0xa67f93ff);
        let blended = lower.oklab_blend(upper);
        assert_eq!(blended, expected);
    }
}
