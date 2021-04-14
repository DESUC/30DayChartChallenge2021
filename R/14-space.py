#!/usr/bin/env python
# coding: utf-8

# importar pandas para leer cvs
import pandas as pd
df = pd.read_csv('datos.csv')
df.head()
# importar matplotlib para gráficos y numpy para pruebas
import matplotlib.pyplot as plt
import matplotlib.path as mpath
import numpy as np
star = mpath.Path.unit_regular_star(5)
circle = mpath.Path.unit_circle()
# concatenar el circulo con la estrella
verts = np.concatenate([circle.vertices, star.vertices[::-1, ...]])
codes = np.concatenate([circle.codes, star.codes])
cut_star = mpath.Path(verts, codes)
# definir colores a usar en el gráfico
colors = ['b', 'r']
# se define figura y márgenes
fig, ax = plt.subplots(figsize=(10,5))
# construcción del gráfico
df.groupby(['Año','País']).sum()['Misiones'].unstack().plot(ax=ax, color=colors, marker=cut_star, markersize=10, linestyle='--', linewidth=1)
plt.style.use('default')
plt.xlabel('Año', fontsize=12)
plt.ylabel('N° de misiones al Espacio', fontsize=12)
plt.title('Misiones al Espacio de Estados Unidos y la Unión Soviética entre 1957 y 1988', fontsize=14)
plt.xlim(1957, 1988)
plt.ylim(-0.5, 8)
plt.annotate('Yuri Gagarin-1961', xy=(1961, 4), xytext=(1959, 5.5), arrowprops=dict(facecolor='red', shrink=0.1), fontsize=8)
plt.annotate('1er sobrevuelo a Marte-1965', xy=(1965, 6), xytext=(1963, 7.3), arrowprops=dict(facecolor='blue', shrink=0.1), fontsize=8)
plt.annotate('1ra estación tripulada-1971', xy=(1971, 5), xytext=(1968, 6.2), arrowprops=dict(facecolor='red', shrink=0.1), fontsize=8)
plt.annotate('Record 84 días en el espacio-1973', xy=(1973, 4), xytext=(1974, 5.5), arrowprops=dict(facecolor='blue', shrink=0.1), fontsize=8)
plt.figtext(0.13, 0.0001, "Fuente: Elaboración propia con datos Korolev and Freedom of Space-NASA", ha="left", fontsize=9, bbox={"facecolor":"red", "alpha":0, "pad":-5})
plt.grid(False)
plt.show()
