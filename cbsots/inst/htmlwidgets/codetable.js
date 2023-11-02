HTMLWidgets.widget({

  name: 'codetable',

  type: 'output',

  factory: function(el, width, height) {

    var BOOLEAN_COL = 1;

    // default callback function for the search plugin
    const DEFAULT_CALLBACK = function(instance, row, col, data, testResult) {
              instance.getCellMeta(row, col).isSearchResult = testResult;
    };
 
    var table_id;
    var table_dim;

    var hot = new Handsontable(el, {
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
        manualColumnResize: true,
        search: {
            callback: search_call_back
        },
        undo: true,
        multiSelect: false,
        outsideClickDeselects : false,
        contextMenu: ['undo', 'redo'],
	afterLoadData: function(initialLoad) {
	    if (initialLoad) {
	       /* initialLoad is TRUE when the hot table is created.
		* It is FALSE when the table is actually rendered. */
               return;
            }
            init_search();
	},
        afterChange: function(changes, source) {
            if (!changes) {
                return;
            }
	    if (HTMLWidgets.shinyMode) {
               Shiny.setInputValue(el.id, {
	               table_id: table_id,
	               dimension: table_dim,
		       data: hot.getData()
	       });
            }
        },
        afterSelection: function(row, column, row2, column2, preventScrolling, selectionLayerLevel) {
	    if (HTMLWidgets.shinyMode) {
               Shiny.setInputValue(el.id + "_selection", {
	               row:     row,
	               column:  column,
		       row2 :   row2,
	               column2: column2
	       });
            }
        },
        licenseKey: "non-commercial-and-evaluation"
    });

    function search_call_back(instance, row, col, value, result) {

        // do not highlight search results in the Select column.

       if (col != BOOLEAN_COL) {
             DEFAULT_CALLBACK.apply(this, arguments);
        }
    }

    /* search_result contains the result of the latest search query.
       search_index is the index in the search_result array that is used to
       implement the Search Next and Search Previous behaviour.
    */
    var search = hot.getPlugin('search');
    var search_result;
    var found_cell_indices;
    var found_cell_indices_rev;
    var new_search_result;
    var ncol = 4;  /* a codetable always contains 4 columns */
    var nrow;
    var ncell;
 
    function init_search() {
        search_result = [];
        found_cell_indices = [];
        found_cell_indices_rev = [];
        new_search_result = false;
        nrow = hot.countRows();
        ncell = ncol * nrow;
    }
 
    init_search();

    // get_cell_index returns the index of a cell in the matrix data,
    // where the matrix data is stored rowwise
    var get_cell_index = function(row, col) {
        return  row * ncol + col;
    };
    
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
    };
    
    
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
            return cell_index >= start_index
        };
    
        index = found_cell_indices.findIndex(fun);
        if (index == -1) {
            index = 0;
        } 
        return search_result[index];
    };
    
    
    // get_prev_search_result returns cell coordinates of the first search result
    // before the selected cell.
    var get_prev_search_result = function() {
    
        start_index = get_selected_index();
        if (start_index == -1) {
            return search_result[search_result.length - 1];
        }
        
        start_index = (ncell + --start_index) % ncell;
    
        var fun = function(cell_index, index, array) {
            return cell_index <= start_index;
        };
    
        index  = found_cell_indices_rev.findIndex(fun);
        if (index == -1) {
            index = search_result.length - 1;
        } else {
            index = search_result.length - 1 - index;
        }
        return search_result[index];
    };

    Handsontable.dom.addEvent(search_field, 'keyup', function(event) {
        if (event.keyCode == 13 && search_result.length > 0) {
	    // Pressed on enter
	
            var cell = get_search_result(!new_search_result);
            hot.selectCell(cell.row, cell.col, cell.row, cell.col, true, 
                           false);
            // Note: the next statement only works for the read-only
            // columns. Therefore it is not possible to change the focus
            // for the Select and Code columns.
            search_field.focus();
            new_search_result = false;
 
        } else {
    
            var search_result_tmp = search.query(this.value);

            // Remove search results in the Select column (col == BOOLEAN_COL). 
            if (search_result_tmp.length > 0) {
                var cnt = 0;
                for (i = 0; i < search_result_tmp.length; i++) {
                   if (search_result_tmp[i].col != BOOLEAN_COL) cnt++;
                }
                search_result = new Array(cnt);
                cnt = 0;
                for (i = 0; i < search_result_tmp.length; i++) {
                   if (search_result_tmp[i].col != BOOLEAN_COL) {
                       search_result[cnt++] = search_result_tmp[i];
                   }
                }
            } else {
               search_result = new Array(0);
           }

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

           // render table again, to all cells will be displayed with
           // colour
           hot.render();
        }
    });
         
    /* 
     * Add listeners to the next and prev buttons
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
     });

    return {

      renderValue: function(x) {
        hot.loadData(x.data);
	table_id = x.table_id;
	table_dim = x.dimension;
	var sel = x.selection;
	console.log(sel);
	console.log(sel.row);
	console.log(sel.column);
	hot.selectCell(sel.row, sel.column, sel.row2, sel.column2, true, false);
      },

      resize: function(width, height) {
      }
    };
  }
});
