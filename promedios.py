import numpy as np

# Datos de ejemplo
grupo1 = [1, 2, 3]
grupo2 = [4, 5, 6]
grupo3 = [7, 8, 9, 10]

# Cálculo del promedio de promedios de subgrupos
promedio_grupo1 = np.mean(grupo1)
promedio_grupo2 = np.mean(grupo2)
promedio_grupo3 = np.mean(grupo3)

promedio_de_promedios = np.mean([promedio_grupo1, promedio_grupo2, promedio_grupo3])

print(f"Promedio del grupo 1: {promedio_grupo1}")
print(f"Promedio del grupo 2: {promedio_grupo2}")
print(f"Promedio del grupo 3: {promedio_grupo3}")
print(f"Promedio de promedios: {promedio_de_promedios}")

# Cálculo del promedio usando todos los datos
todos_los_datos = grupo1 + grupo2 + grupo3
promedio_total = np.mean(todos_los_datos)

print(f"\nPromedio usando todos los datos: {promedio_total}")

# Comparación
print(f"\n¿Son iguales? {'Sí' if promedio_de_promedios == promedio_total else 'No'}")
print(f"Diferencia: {abs(promedio_de_promedios - promedio_total)}")