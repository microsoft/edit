FROM bitnami/rust

ARG ICU_VERSION=74

ENV EDIT_CFG_ICU_RENAMING_VERSION=${ICU_VERSION}
ENV EDIT_CFG_ICUUC_SONAME=libicuuc.so.${ICU_VERSION}
ENV EDIT_CFG_ICUI18N_SONAME=libicui18n.so.${ICU_VERSION}

COPY . .

RUN cargo build --config .cargo/release.toml --release
RUN cp -v ./target/release/edit /edit.gnu.icu74


