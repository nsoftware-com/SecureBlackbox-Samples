$(document).ready(function(){
    assignButtons();
    loadData();
});

function assignButtons() {
    $("#submitButton").click(function (e) {
        var val = $("#taskText").val();
      
        if (val == "")
            $("#textSpan").text("Enter text");
        else
            $("#textSpan").text("");

        if (val != "") {
            $.ajax({
                type: "POST",
                url: "/items/add",
                contentType: "text/plain",
                data: val,
                dataType: "text"
            });
            
            window.location = "/";
        }
    });

    $('#deleteButton').click(function() {
        var idList = { 'delete' : []};

        $(":checked").each(function() {
            idList['delete'].push($(this).val());
        });
        
        $.ajax({
            type: "DELETE",
            url: "/items/delete",
            contentType: "text/plain",
            data: idList,
            dataType: "text"
        });
            
        window.location = "/";
    });
}

function loadData() {
    $("#data > tbody").empty();

    $.getJSON('/items', function(data) {
        var i;

        for (i = 0; i < data.length; i++) {
            $('#data > tbody:last-child').append(
                $('<tr>')
                    .append(
                        $('<td>').append(
                            $('<input>').attr('type', 'checkbox').attr('value', data[i].id)
                        )
                    )
                    .append($('<td>').append(data[i].id))
                    .append($('<td>').append(data[i].value))
            );
        }
    });
}
