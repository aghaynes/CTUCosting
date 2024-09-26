# CTUCosting 0.6.0 (2024-09-26)

* restyle to approximate DCR letterhead, including DCR logo
* switch email addresses to admin.dcr

# CTUCosting 0.5.4 (2024-07-16)

* update SNF template to 2024 rates
* add number of units and cost per unit to the expenses form in admin info excel file

# CTUCosting 0.5.3 (2024-06-03)

* update head of intensive medicine

# CTUCosting 0.5.2 (2024-06-03)

* change sponsor to customer in admin info
* include task level info in admin info
* update neurology head to Urs Fischer

# CTUCosting 0.5.1 (2024-05-21)

* bug fix: xlsx files for SNF projects with expenses failed (8415b16).
* Universitätsklinik für Viszerale Chirurgie und Medizin split into two units: 'Viszerale und transplantationschirurgie' and 'Gastroenterologie' with separate clinic directors.

# CTUCosting 0.5.0 (2024-04-18)

* Add support for costings from the DM reporting group.
* Refer to Institution rather than Sponsor on the costing PDF.
* MZ noticed that the filename in adobe is sometimes shown as "A test document". This update changes it to the filename instead.

# CTUCosting 0.4.9 (2024-04-10)

* Bug fix: app crashed if only FTEs were entered.

# CTUCosting 0.4.8

* Changes to clinic names:
  * Pneumologie -> Pneumologie, Allergologie und klinische Immunologie
  * Rheumatology, immunologie und allergology -> rheumatologie und immunologie

# CTUCosting 0.4.7

* document development and regulatory affairs should use CSM rate rather than the MON rate

# CTUCosting 0.4.6

* bug fix: filtering of notes did not work correctly when there were no notes (bug introduced in v0.4.5).
* bug fix: discount row should not be shown for SNF projects.
* Improved alignment of cost column in PDF output

# CTUCosting 0.4.5

* University overhead only relevant for EXTERNAL FOR-PROFIT projects.
* Filter notes when work packages are filtered.

# CTUCosting 0.4.4

* Bug fix: Shiny app errored if the number of years was missing.
* Add filter for FTE positions. 
* Add signatories for a few institutes.
* Include FTE info in admin information.
* Support for General Support work package. Improved support for generic form.
* Styling of the cost column in the PDF adapted - right align, rounding, and consistent thousands separator.
* All costs rounded to the nearest CHF, note added to PDF.
* Note that the discount is not applied to all packages added to PDF.
* App now generates an error message if a work package is not entered (which previously resulted in the app crashing).
* Include 'Amendment_project number' in PDF and admin file names if the project number is available.
* Bug fix: SNF table generation failed when a single work package was involved.
* Improve test coverage from ca 50% to >80% (according to `covr`)

# CTUCosting 0.4.3

* `totals` updated (Internal PM not relevant for SNF projects).
* Option to add remove page break between first and second page added to UI/Rmd template.
* No notes resulted in a horizontal rule in PDF. That has been fixed.

# CTUCosting 0.4.2

* Setting Urs as clinic head was premature. Reset to Claudio.
* Minor internal documentation changes to pass `R CMD CHECK`.
* Add UNIBE favicon.
* Add the version number to the bottom of the sidebar.

# CTUCosting 0.4.1

* Update Neurology clinic head (Urs Fischer).

# CTUCosting 0.4.0

* Initial version.
