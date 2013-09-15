# Lensed Forms

## Convention for fields

All a field needs to have, as a default, is:

`<input name="FIELD_NAME-input">`

## Convention for Sub-Forms (Many and Record)

For a sub-form named `NAME`: 

`
<div name="add-NAME">

  <!-- MANY ONLY: the name of the sub-form, with number for iteration -->
  <h3>NAME #<span name="NAME-number">0</span></h3>

  <!-- needed on all sub-forms -->
  <span data-lift="embed?what=_add_NAME_form"></span>

  <!-- MANY ONLY -->
  <p>[ <span name="NAME-add">+</span> | <span name="NAME-remove">-</span> ]</p>

</div>

<!-- MANY ONLY -->
<div id="NAME-elements"></div>
`

The idea is it keep only the most basic things in the embeded form (i.e. fields). The rest of the chrome resides outside. 
