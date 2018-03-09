HTMLWidgets.widget({

  name: 'codetable',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {
    
        var BOOLEAN_COL = 1;
 
	    var hot = new Handsontable(el, {
            data: x.data,
            rowHeaders: true,
            colHeaders: [
                   "Key",
                   "Select",
                   "Code",
                   "Title"],
            columns: [
                {data: "Key", type: 'text', readOnly : true},
                {data: "Select", type: 'checkbox'},
                {data: "Code", type : 'text'},
                {data: "Title", type : 'text', readOnly : true}
            ],
            filters: true,
            dropdownMenu: true,
            height: 500,
            wordWrap: false,
            search: {
                callback: search_call_back
            },
            undo: true,
            multiSelect: false,
            outsideClickDeselects : false,
            contextMenu: ['undo', 'redo'],
            afterChange: function(changes, source) {
                if (!changes) {
                    return;
                }
                Shiny.onInputChange(el.id, hot.getData());
            }
         });

         // initialise data
         Shiny.onInputChange(el.id, hot.getData());

         function search_call_back(instance, row, col, value, result) {
         //do not show search results in the column with logical values -->
             if (col != BOOLEAN_COL) {
                 Handsontable.plugins.Search.DEFAULT_CALLBACK.apply(this, arguments);
             }
         };


         /* search_result contains the result of the latest search query.
            search_index is the index in the search_result array that is used to
            implement the Search Next and Search Previous behaviour.
         */
         var search_result = [];
         var found_cell_indices = [];
         var found_cell_indices_rev = [];
         var new_search_result = false;
         var ncol = hot.countCols();
         var nrow = hot.countRows();
         var ncell = ncol * nrow;
         
         // get_cell_index returns the index of a cell in the matrix data,
         // where the matrix data is stored rowwise
         var get_cell_index = function(row, col) {
             return  row * ncol + col;
         }
         
         // get_selected_index returns the index of the currently selected cell
         // (see comments above for function get_cell_index), or -1 is no cell
         // is selected.
         var get_selected_index = function() {
             var sel = hot.getSelected();
             if (typeof sel == "undefined") {
                 return -1;
             } else {
                 return get_cell_index(sel[0][0], sel[0][1]);
             }
         }
         
         
         // get_search_result returns the cell coordinates of
         // the first search result for the cells after the selected cell.
         // If nxt == false and the selected cell is a search result,
         // it returns the coordinates of the selected cell.
         var get_search_result = function(nxt) {
             
             start_index = get_selected_index();
             if (start_index == -1) {
                 return search_result[0];
             }
         
             if (nxt) {
                 start_index = ++start_index % ncell;
             }
         
             var fun = function(cell_index, index, array) {
                 return cell_index >= start_index  &&
                        search_result[index].col != BOOLEAN_COL;
             }
         
             index = found_cell_indices.findIndex(fun);
             if (index == -1) {
                 index = 0;
             } 
             return search_result[index];
         }
         
         
         // get_prev_search_result returns cell coordinates of the first search result
         // before the selected cell.
         var get_prev_search_result = function() {
         
             start_index = get_selected_index();
             if (start_index == -1) {
                 return search_result[search_result.length - 1];
             }
             
             start_index = (ncell + --start_index) % ncell;
         
             var fun = function(cell_index, index, array) {
                 return (cell_index <= start_index  && 
                        found_cell_indices_rev[search_result.length - 1 - index].col 
                        != BOOLEAN_COL);
             };
         
             index  = found_cell_indices_rev.findIndex(fun);
             if (index == -1) {
                 index = search_result.length - 1;
             } else {
                 index = search_result.length - 1 - index;
             }
             return search_result[index];
         }
         
         Handsontable.dom.addEvent(search_field, 'keyup', 
                 function (event) {
                     if (event.code == "Enter" && search_result.length > 0) {
                         var cell;
                         if (!new_search_result) {
                             cell = get_search_result(true);
                         } else {
                             cell = get_search_result(false);
                         }
                         hot.selectCell(cell.row, cell.col, cell.row, cell.col, true, 
                                        false);
                         search_field.focus();
                         new_search_result = false;
         
                     } else {
         
                         search_result = hot.search.query(this.value);
         
                         if (search_result.length > 0) {
         
                             // administration for the cell indices of found cells
                             found_cell_indices = new Array(search_result.length);
                             for (i = 0; i < search_result.length; i++) {
                                 found_cell_indices[i] = get_cell_index(search_result[i].row,
                                                                            search_result[i].col);
                             }
                             found_cell_indices_rev = found_cell_indices.slice();
                             found_cell_indices_rev.reverse();
         
                             new_search_result = true;
                             var cell = get_search_result(false);
                             hot.scrollViewportTo(cell.row, cell.col);
                         } 
                         hot.render();
                     }
                 });
         
               /* 
                * Add listeners to thee buttons
                */
               
               var next_button = document.getElementById("next_button");
               var prev_button = document.getElementById("prev_button");
         
               next_button.addEventListener("click", function(event) {
                   if (search_result.length > 0) {
                       cell = get_search_result(true);
                       hot.selectCell(cell.row, cell.col, cell.row, cell.col, true, false);
                       new_search_result = false;
                   }
               });
         
               prev_button.addEventListener("click", function(event) {
                   if (search_result.length > 0) {
                       cell = get_prev_search_result();
                       hot.selectCell(cell.row, cell.col, cell.row, cell.col, true, false);
                       new_search_result = false;
                   }
               })

      },

      resize: function(width, height) {

      }

    };
  }
});
