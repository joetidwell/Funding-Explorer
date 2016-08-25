function caretSwitch(id) {
    el = document.getElementById(id)
    // alert(id)
    if(el.className == 'fa fa-caret-square-o-down') {
        el.className = 'fa fa-caret-square-o-right';
    } else {
        el.className = 'fa fa-caret-square-o-down';
    }

}