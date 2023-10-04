HTMLWidgets.widget({

  name: 'codetable',

  type: 'output',

  factory: function(el, width, height) {

    var data_init = [
	  {Key: 'X', Selected: true, Code: 'A', Title: 'Hello'}
	];

    var BOOLEAN_COL = 1;

    // default callback function for the search plugin
    const DEFAULT_CALLBACK = function(instance, row, col, data, testResult) {
              instance.getCellMeta(row, col).isSearchResult = testResult;
    };

    var hot = new Handsontable(el, {
        data: data_init,
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
        hiddenColumns: {
            columns: ["table_id"],
            indicators: true
        },
        afterChange: function(changes, source) {
            if (!changes) {
                return;
            }
            Shiny.setInputValue(el.id, hot.getData());
        },
        licenseKey: "non-commercial-and-evaluation"
    });

    function search_call_back(instance, row, col, value, result) {

        // do not highlight search results in the Select column.

        if (col != BOOLEAN_COL) {
             DEFAULT_CALLBACK.apply(this, arguments);
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

    var search = hot.getPlugin('search');

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
            return cell_index >= start_index
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
            return cell_index <= start_index;
        };
    
        index  = found_cell_indices_rev.findIndex(fun);
        if (index == -1) {
            index = search_result.length - 1;
        } else {
            index = search_result.length - 1 - index;
        }
        return search_result[index];
    }

    return {

      renderValue: function(x) {
        hot.loadData(x.data);
/*	  data: x.data
	});  */
	/* TODO: check why the if statement below is necessary */
	/* TODO: try handsontable afterLoadData */
	if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue(el.id, hot.getData());
        }

	/* this code must be here, for unknown reasons  and not in the main body for unknown reaons */
        /*Handsontable.dom.addEvent(search_field, 'keyup', function (event) {

        });  */

      },
    
      resize: function(width, height) {
      }
    };
  }
});
