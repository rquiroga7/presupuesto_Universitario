$headers = @{
    "Authorization" = "cbbd85c1-1986-4491-a5f6-8de8f4deb733"
    "Content-Type" = "application/json"
}

$body = @'
{
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
        2025
    ],
    "filters": [
        {
            "column": "programa_id",
            "value": "26",
            "operator": "equal"
        },
        {
            "column": "programa_desc",
            "value": "Desarrollo de la Educacion Superior",
            "operator": "equal"
        }
    ]
}
'@

$response = Invoke-WebRequest -Method POST -Uri "https://www.presupuestoabierto.gob.ar/api/v1/credito?format=json" -Headers $headers -Body $body
# Save raw response to a temporary file so we preserve the exact response bytes
[System.IO.Directory]::CreateDirectory("datos")
$tempFile = "datos/2025_raw.json"
Invoke-WebRequest -Method POST -Uri "https://www.presupuestoabierto.gob.ar/api/v1/credito?format=json" -Headers $headers -Body $body -OutFile $tempFile

# Read bytes and remove UTF-8 BOM if present, then write final file (UTF-8 without BOM)
$bytes = [System.IO.File]::ReadAllBytes($tempFile)
if ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
    $bytes = $bytes[3..($bytes.Length - 1)]
}
[System.IO.File]::WriteAllBytes("datos/2025.json", $bytes)