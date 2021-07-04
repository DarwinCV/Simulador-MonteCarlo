observeEvent(input$tour, {
  introjs(
    session,
    options = list(
      steps = list(
        list(
          element = "#tour_1",
          intro = "Este boton ejecuta la simulacion de frecuencia - severidad.
          Haz clic 'Simular' y luego haz clic en el boton 'Next'"
        ),
        list(
          element = "#tour_2",
          intro = "Este histograma muestra la distribucion de las perdidas finales por observacion de frecuencia / severidad. 
          La  perdida por observacion se encuentra en el eje x y el eje y el numero de observaciones."
        ),
        list(
          element = "#tour_3",
          intro = "Este deslizador ajusta el numero de observaciones de frecuencia-severidad que ejecuta la simulacion. 
          Una observacion es una frecuencia simulada (es decir, numero de reclamos) con una gravedad simulada para cada reclamo. 
          (por ejemplo, una observacion podria tener 9 reclamos (frecuencia) cada uno con diferentes cantidades de perdida (severidades) ."
        ),
        list(
          element = "#tour_4",
          intro = "La distribucion de frecuencia genera aleatoriamente el numero de reclamos en cada observacion. 
          Para obtener mas informacion sobre estas distribuciones, consulte el Apendice B de 
          https://www.soa.org/files/pdf/edu-2009-fall-exam-c-table.pdf"
        ),
        list(
          element = "#tour_5",
          intro = "La distribucion de la gravedad genera aleatoriamente el monto final en dolares para liquidar cada reclamo.
            Para obtener mas informacion sobre estas distribuciones, 
          consulte el Apendice A de https://www.soa.org/files/pdf/edu-2009-fall-exam-c-table.pdf"
        ),
        list(
          element = "#tour_6",
          intro = "Las aseguradoras a menudo compran polizas de reaseguro en exceso para limitar su exposicion a grandes perdidas.
          Dos formas comunes para que una aseguradora limite su exposicion a grandes perdidas son 
          'por limite de reclamo' y 'por limite agregrado' "
        ),
        list(
          element = "#tour_7",
          intro = "El 'limite de reclamo' limita la perdida retenida en un reclamo individual,
          p. si un asegurador tiene una poliza de exceso con un limite de 250,000 por reclamo, 
          el maximo que pagara el asegurador por ese reclamo es de 250,000"
        ),
        list(
          element = "#tour_8",
          intro = "El 'limite agregado' (tambien denominado 'perdida de parada agregada') establece 
          un limite superior en la cantidad que la aseguradora puede perder en una observacion de frecuencia-severidad."
        ),
        list(
          element = "#tour_9",
          intro = "Ajusta el nivel de confianza que se muestra en las graficas"
        ),
        list(
          element = "#tour_10",
          intro = "Este grafico muestra las perdidas finales por reclamo bruto de los limites de retencion seleccionados. 
          A diferencia de la trama superior que es neta de los limites de retencion."
        ),
        list(
          element = ".nav-tabs",
          intro = "Por ultimo, pero no menos importante, puede hacer clic en estas secciones para ver una tabla de la salida
          y descargar todos los datos simulados"
        )
      )
    )
  )
})