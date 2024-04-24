curl -X POST \
  https://www.presupuestoabierto.gob.ar/api/v1/credito?format=json \
  -H 'Authorization: cbbd85c1-1986-4491-a5f6-8de8f4deb733' \
  -H 'Content-Type: application/json' \
  -d '{
    "columns": [
        "impacto_presupuestario_fecha", 
        "impacto_presupuestario_anio", 
        "impacto_presupuestario_mes", 
        "ejercicio_presupuestario", 
        "sector_id", 
        "sector_desc", 
        "subsector_id", 
        "subsector_desc", 
        "caracter_id", 
        "caracter_desc", 
        "jurisdiccion_id", 
        "jurisdiccion_desc", 
        "subjurisdiccion_id", 
        "subjurisdiccion_desc", 
        "entidad_id", 
        "entidad_desc", 
        "servicio_id", 
        "servicio_desc", 
        "programa_id", 
        "programa_desc", 
        "subprograma_id", 
        "subprograma_desc", 
        "proyecto_id", 
        "proyecto_desc", 
        "actividad_id", 
        "actividad_desc", 
        "obra_id", 
        "obra_desc", 
        "finalidad_id", 
        "finalidad_desc", 
        "funcion_id", 
        "funcion_desc", 
        "inciso_id", 
        "inciso_desc", 
        "principal_id", 
        "principal_desc", 
        "parcial_id", 
        "parcial_desc", 
        "subparcial_id", 
        "subparcial_desc", 
        "clasificador_economico_8_digitos_id", 
        "clasificador_economico_8_digitos_desc", 
        "fuente_financiamiento_id", 
        "fuente_financiamiento_desc", 
        "ubicacion_geografica_id", 
        "ubicacion_geografica_desc", 
        "unidad_ejecutora_id", 
        "unidad_ejecutora_desc", 
        "prestamo_externo_id", 
        "prestamo_externo_desc", 
        "codigo_bapin_id", 
        "codigo_bapin_desc", 
        "credito_presupuestado", 
        "credito_vigente", 
        "credito_comprometido", 
        "credito_devengado", 
        "credito_pagado", 
        "ultima_actualizacion_fecha"
    ],
    "ejercicios": [
        2024
    ],
    "filters": [
        {
            "column": "programa_id",
            "value": "26",
            "operator": "equal"},
        {
            "column": "programa_desc",
            "value": "Desarrollo de la Educacion Superior",
            "operator": "equal"}   
    ]
}' > 2024.json
