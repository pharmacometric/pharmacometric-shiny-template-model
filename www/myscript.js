//include stylesheet if not included
var link = document.createElement( "link" );
link.href = "myscript.css";
link.type = "text/css";
link.rel = "stylesheet";
link.media = "screen,print";


// Function to update the list of checked models
var checkedModels = [];
function updateCheckedModels() {
    // Get all the checkboxes within the table
    const checkboxes = document.querySelectorAll('#modelrunmain input.modelruncompare[type="checkbox"]');

    // Collect the model attribute values of checked checkboxes
    checkedModels = [];
    checkboxes.forEach(checkbox => {
        if (checkbox.checked) {
            checkedModels.push(checkbox.getAttribute('model'));
        }
    });

    // Log the current list of checked model values to the input
    Shiny.setInputValue("jsselectedmodels", checkedModels);
}

function waitForElm(selector) {
    return new Promise(resolve => {
        if (document.querySelector(selector)) {
            return resolve(document.querySelector(selector));
        }

        const observer = new MutationObserver(mutations => {
            if (document.querySelector(selector)) {
                observer.disconnect();
                resolve(document.querySelector(selector));
            }
        });

        // If you get "parameter 1 is not of type 'Node'" error, see https://stackoverflow.com/a/77855838/492336
        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    });
}



waitForElm('#modelrunmain').then((elm) => {
    // sort table
const table = document.getElementById('modelrunmain');
    const headers = table.querySelectorAll('th');
    let currentSortColumn = -1;
    let currentSortOrder = 'asc';

    table.addEventListener('click', () => {
            Shiny.setInputValue("activatemodelbuttons", "activate model buttons");
        });
    headers.forEach((header, index) => {
        header.addEventListener('click', () => {
            sortTable(index);
        });
    });

    function sortTable(columnIndex) {
        const rows = Array.from(table.rows).slice(1); // Exclude header row
        const isAscending = (currentSortColumn === columnIndex) ? currentSortOrder === 'asc' : true;
        const sortDirection = isAscending ? 1 : -1;

        rows.sort((rowA, rowB) => {
            const cellA = rowA.cells[columnIndex].innerText.trim();
            const cellB = rowB.cells[columnIndex].innerText.trim();

            // Sort logic for different data types
            if (columnIndex === 3) { // Count (integer)
                return (parseInt(cellA) - parseInt(cellB)) * sortDirection;
            } else if (columnIndex === 2) { // For progress
                return (parseInt(rowA.cells[2].querySelector('.progress-bar').style.width) -
                        parseInt(rowB.cells[2].querySelector('.progress-bar').style.width)) * sortDirection;
            } else { // For text
                return (cellA.localeCompare(cellB)) * sortDirection;
            }
        });

        // Append sorted rows back to table body
        rows.forEach(row => table.tBodies[0].appendChild(row));

        // Update current sort column and order
        currentSortColumn = columnIndex;
        currentSortOrder = isAscending ? 'desc' : 'asc';
    }







Shiny.addCustomMessageHandler("activatejsselectedmodels", function(m) {
console.log(m)
updateCheckedModels()
/* Add event listeners to each checkbox
document.querySelectorAll('#modelrunmain input.modelruncompare[type="checkbox"]').forEach(checkbox => {
    checkbox.addEventListener('change', updateCheckedModels);
    console.log("input,")
});
*/

});


});





document.getElementsByTagName( "head" )[0].appendChild( link );

document.addEventListener("DOMContentLoaded", function(){



document.querySelectorAll('.jarviswidget .widget-body .padding-15').forEach(function(e) {
   e.style = "overflow-x: auto"
});

//include shadow for cards if not included
document.querySelectorAll('.jarviswidget').forEach(function(e) {
   // e.className = e.className + " shadow"
});


/*

/ Use querySelector to select the image within the div with ID plotgofsui
        const image1 = document.querySelector('#plotgofsui img');

        // Create a MutationObserver to watch for attribute changes
        const observer = new MutationObserver((mutations) => {
            mutations.forEach(mutation => {
                if (mutation.type === 'attributes' && mutation.attributeName === 'src') {
                    console.log('Image src changed to:', image1.src);
                }
            });
        });

        // Start observing the image for attribute changes
        observer.observe(image1, {
            attributes: true // We want to observe attribute changes
        });










            const pre = document.getElementById('code-block');
            const code = pre.querySelector('code');
            const lines = code.textContent.trim().split('\n');

            const lineNumbering = document.createElement('div');
            lineNumbering.className = 'line-numbers';

            lines.forEach((line, index) => {
                const lineNumber = document.createElement('div');
                lineNumber.textContent = (index + 1).toString();
                lineNumbering.appendChild(lineNumber);
            });

            pre.insertBefore(lineNumbering, code);

            */

})
