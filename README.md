This code runs an applicatio for the calculation of the CARE-FI (Composite Administrative Data-based Ranking Estimator Frailty Index) frailty indicator for the resident population aged 65 and older within the territory of a health authority. It is designed for Italian health data and developed entirely in Italian.

# Applicativo Indicatore di Fragilità

Versione aggiornata al 04/06/26

## Introduzione all’applicativo

La presente applicazione consente il calcolo dell’indicatore di fragilità CARE-FI (Composite Administrative data-based Ranking Estimator Frailty Index), riferito alla popolazione residente di età pari o superiore a 65 anni nel territorio di un’Azienda Sanitaria.

Il CARE-FI è costruito esclusivamente a partire dalle informazioni contenute nei database amministrativi delle ASL relativi a due annualità consecutive e si basa su otto dimensioni ritenute rilevanti ai fini della valutazione della fragilità: età, presenza di disabilità, numero di ospedalizzazioni, disturbi psichici, malattie del sistema nervoso, insufficienza cardiaca, insufficienza renale e cancro attivo.

Il calcolo dell’indicatore avviene mediante la teoria degli insiemi parzialmente ordinati (POSET), un approccio non parametrico che consente di ordinare gli individui sulla base delle otto caratteristiche considerate, senza la necessità di definire a priori pesi per le singole variabili. In questo modo, il CARE-FI fornisce una misura sintetica della fragilità basata sull’insieme delle condizioni di salute rilevate nei dati amministrativi.

L’applicazione elabora automaticamente l’indicatore di fragilità e aggiunge al file caricato una colonna contenente il valore di CARE-FI per ciascun soggetto. Inoltre, consente di esplorare le relazioni tra fragilità e condizioni cliniche; e di calcolare il livello di fragilità per un nuovo individuo rispetto alla popolazione di riferimento.

Per ulteriori dettagli relativi alla costruzione metodologica dell’indicatore si rimanda al seguente link: https://arxiv.org/abs/2506.23158.
