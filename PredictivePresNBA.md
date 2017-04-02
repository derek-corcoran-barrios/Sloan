Modelos espaciales para la NBA
========================================================
author: Derek Corcoran
date: "2017-04-02"

La revolucion estadística en la NBA
========================================================
left: 60%

**Philadelphia 76ers:**
<small>
* Dr. Lance Pearson (Coordinator of Coaching Analytics)
* Sergi Oliva (Director of Basketball Analytics & Innovation)
* Alex Rucker (Vice President - Analytics and Strategy)
* Andy Miller (Senior Researcher)
* Alex D’Amour (Senior Researcher)
* Alex Franks (Senior Researcher)
* Grant Fiddyment (Data Scientist)
* Michael Lai (Data Scientist) 
.</small>

***

![alt text](51KkoapPY2L._AC_UL320_SR216,320_.jpg)

=======================================================
incremental: true

![alt text](http://www.sloansportsconference.com/wp-content/uploads/2016/02/MIT-Sloan-Sports-Analytics-Conference.jpg)


* Congreso anual en MIT en Boston
* 11 años
* 4.000 participantes  o
* 1.500 estudiantes de 320 instituciones academicas 
* 130 equipos profesionales 


=============================================


<div align="center">
<img src="http://www.obsessedwithsports.com/wp-content/uploads/2013/03/revenge-of-the-nerds-sloan-conference.png" width=900 height=550>
</div>


Que es SportsVU
========================================================
incremental: true
left: 60%







![alt text](SportVU.jpg)

***
<small>

- Seguimiento de jugadores en tiempo real con 6 camaras
- Información registrada 25 veces por segundo
- Algunas de los valores registrados
    + Posición
    + Velocidad
    + Distancia a otros jugadores
    + Pases
    + Tipos de tiro
.</small>

Esto permite seguimientos como este
========================================================

<div align="center">
<img src="spurs_movement5_part2.gif" width=900 height=550>
</div>


y generar datos como este:
========================================================

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> PLAYER_NAME </th>
   <th style="text-align:left;"> SHOT_TYPE </th>
   <th style="text-align:right;"> SHOT_DISTANCE </th>
   <th style="text-align:right;"> LOC_X </th>
   <th style="text-align:right;"> LOC_Y </th>
   <th style="text-align:left;"> SHOT_MADE_FLAG </th>
   <th style="text-align:left;"> HTM </th>
   <th style="text-align:left;"> VTM </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Andre Drummond </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -17 </td>
   <td style="text-align:right;"> -6 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marcus Morris </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paul Millsap </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kentavious Caldwell-Pope </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> -68 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Al Horford </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> -117 </td>
   <td style="text-align:right;"> 164 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Andre Drummond </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> -79 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paul Millsap </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 123 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jeff Teague </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> -2 </td>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reggie Jackson </td>
   <td style="text-align:left;"> 3PT Field Goal </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> -25 </td>
   <td style="text-align:right;"> 239 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jeff Teague </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Al Horford </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> -65 </td>
   <td style="text-align:right;"> 169 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Andre Drummond </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> -9 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jeff Teague </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Andre Drummond </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Al Horford </td>
   <td style="text-align:left;"> 2PT Field Goal </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> -30 </td>
   <td style="text-align:right;"> 183 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> ATL </td>
   <td style="text-align:left;"> DET </td>
  </tr>
</tbody>
</table>

========================================================


<img src="PredictivePresNBA-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />


========================================================

<img src="PredictivePresNBA-figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />


N
========================================================

![plot of chunk unnamed-chunk-6](PredictivePresNBA-figure/unnamed-chunk-6-1.png)

equaciones
=========================

$PPS = \frac{Points made}{Shots taken}$

========================================================

![plot of chunk unnamed-chunk-7](PredictivePresNBA-figure/unnamed-chunk-7-1.png)

Spread
========================================================

![plot of chunk unnamed-chunk-8](PredictivePresNBA-figure/unnamed-chunk-8-1.png)


========================================================


<img src="PredictivePresNBA-figure/unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />
