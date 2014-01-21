# Lensed Forms

## Convention for fields

All a field needs to have, as a default, is:

`<input name="FIELD_NAME-input">`

## Convention for Sub-Forms (Many and Record)

For a sub-form named `NAME`: 


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


The idea is it keep only the most basic things in the embeded form (i.e. fields). The rest of the chrome resides outside. 

## Convention for Typeahead Forms

There now includes a TYpeahead form for data types that have a One-To-Many relationship with another data type. These forms produce a typeahead form field that will search an API end-point for an item using Twitter's [Typeahead](http://twitter.github.io/typeahead.js/) library. It makes the following assumptions:

- Datums returned from typeahead include an `id` member. 
- You provide a lens `Lens[String, A]` where the string is the id member of the datum (quite often some kind of id).
- Given the `name` of your `TypeaheadField` you use the following template in your form:

    <div name="NAME">
      <div data-lift="embed?what=_typeahead"></div>
    </div>

- You also provide a method to do the side effect for when you need to add a new ember (i.e. when the typeahead search doesn't result in anything).
