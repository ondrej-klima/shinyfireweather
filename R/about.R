AboutUi <- function(id) {
  bs4Dash::box(title = "KSpredict v0.0.1", width = 12,
    shinybusy::add_busy_spinner(spin = "fading-circle"),
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(2, htmltools::img(src="./figures/sticker.png", align="left", width=100)),
        shiny::column(10, htmltools::tagList(htmltools::HTML(
          paste0(
          "<b>Ondřej Klíma<sup>1</sup>, Jiří Neubauer<sup>2</sup>, Tomáš Zeman<sup>3</sup>, Miroslav Králík<sup>4</sup>, Lenka Polcerová<sup>4</sup></b>",
          "<br><br><sup>1</sup>Vysoké učení technické v Brně, Fakulta informačních technologií<br>",
          "<sup>2</sup>Univerzita obrany, Fakulta vojenského leadershipu<br>",
          "<sup>3</sup>Univerzita Tomáše Bati ve Zlíně, Fakulta logistiky a krizového řízení<br>",
          "<sup>4</sup>Masarykova univerzita, Přírodovědecká fakulta<br><br>"
          )),
          htmltools::a("https://github.com/ondrej-klima/shinyfireweather/", href="https://github.com/ondrej-klima/shinyfireweather/")
          ))
      )
    )
  )
}
