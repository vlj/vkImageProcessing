
@(
    "meanAandBPass1.h.spv",
    "meanAandBPass2.h.spv",
    "guidedFilterFinal.h.spv",
    "horizontalSumation.h.spv",
    "verticalSumation.h.spv",
    "guidedFilterPass1.h.spv",
    "guidedFilterPass2.h.spv",
    "averagingISqI.h.spv",
    "test_average.h.spv",
    "copy.h.spv"
    ) | %{
        $filename = $_
        $path = Join-Path $PSScriptRoot "out\build\x64-Debug (default)\lib\ShaderCollection\$filename";
        echo ("Converting " + $path)
        & '.\shaders2headers\target\debug\shader2headers.exe' --input $path --output_folder (Join-Path $PSScriptRoot "lib\ShaderCollection\include\generated")
}