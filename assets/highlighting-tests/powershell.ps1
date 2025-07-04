# Single-line comment

<#
Multi-line
comment
#>

function Get-SampleData {
    param(
        [string]$Name = "World", # String literal, parameter
        [int]$Count = 3
    )

    $array = @(1, 2, 3) # Array literal
    $hashtable = @{ Key1 = 'Value1'; Key2 = 42 } # Hashtable literal

    $nullVar = $null
    $boolTrue = $true
    $boolFalse = $false

    $regexMatch = "abc123" -match '\d+' # Regex literal

    for ($i = 0; $i -lt $Count; $i++) {
        Write-Host "Hello, $Name! Iteration: $i" # Variable interpolation, string
    }

    if ($hashtable.Key2 -eq 42) {
        Write-Output "Hashtable value is 42"
    }
    elseif ($hashtable.Key2 -gt 40) {
        Write-Output "Hashtable value is greater than 40"
    }
    else {
        Write-Output "Hashtable value is less than or equal to 40"
    }

    switch ($Name) {
        "World" { Write-Host "Default name used." }
        default { Write-Host "Custom name: $Name" }
    }

    try {
        throw "An error occurred"
    }
    catch {
        Write-Warning $_
    }
    finally {
        Write-Verbose "Finally block executed"
    }

    $script:globalVar = 99 # Scope modifier

    # Here-String
    $hereString = @"
This is a here-string.
Name: $Name
"@

    return $hereString
}

# Command invocation, pipeline, splatting
$paramSplat = @{
    Name  = 'PowerShell'
    Count = 2
}
Get-SampleData @paramSplat | Out-File -FilePath "./output.txt"

# Type literal, member access, method call
[System.DateTime]::Now.ToString("yyyy-MM-dd")

# Subexpression
Write-Host "2 + 2 = $($array[0] + $array[1])"

# Command substitution
$pwdPath = $(Get-Location).Path
Write-Host "Current directory: $pwdPath"
