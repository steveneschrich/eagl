color_maps <- list(
  ecla_q = c(
    Q1 = "#256297",
    Q2 = "#e26a24",
    Q3 = "#298e3d",
    Q4 = "#bb2727",
    Q5 = "#7c558f",
    Q6 = "#73473e",
    Q7 = "#c36797"
  ),

  ecla_superpopulation = c(
    EUR = "#804c42",
    AFR = "#d02225",
    AMR = "#885db4",
    SAS = "#fe8e8c",
    EAS = "#bea6d0"
  ),

  ecla_population = c(
    MSL = "#787878",
    GWD = "#BC928A",
    CHB = "#94D4E2",
    CHS = "#FE7613",
    KHV = "#BEA6D0",
    CDX = "#289626",
    LWK = "#A4C0E4",
    PJL = "#885CB4",
    GIH = "#9CDE8E",
    ITU = "#B4B41E",
    BEB = "#F6AECC",
    TSI = "#289626",
    IBS = "#D02224",
    PUR = "#FEB573",
    FIN = "#A4C0E4",
    MXL = "#FD933E",
    CLM = "#90DB81",
    JPT = "#1C6CAC",
    GBR = "#1D6CAB",
    CEU = "#D6D682",
    ASW = "#FF730F",
    ACB = "#814C42",
    YRI = "#27BACC",
    ESN = "#DE6CBA",
    PEL = "#FE8E8C",
    STU = "#C0C0C0"

  )

)
color_palettes <- list(
  pong = c(
    "#E04B4B",
    "#6094C3",
    "#63BC6A",
    "#A76BB2",
    "#F0934E",
    "#FEFB54",
    "#B37855",
    "#EF91CA",
    "#A4A4A4"
  ),

  pong_26 <- c(
    "#f0a3ff",
    "#0075dc",
    "#993f00",
    "#4c005c",
    "#191919",
    "#005c31",
    "#2bce48",
    "#ffcc99",
    "#808080",
    "#94ffb5",
    "#8f7c00",
    "#9dcc00",
    "#c20088",
    "#003380",
    "#ffa405",
    "#ffa8bb",
    "#426600",
    "#ff0010",
    "#5ef1f2",
    "#00998f",
    "#e0ff66",
    "#740aff",
    "#990000",
    "#ffff80",
    "#ffff00",
    "#ff5005"
  )
)

usethis::use_data(color_maps,overwrite=TRUE)
usethis::use_data(color_palettes,overwrite=TRUE)
