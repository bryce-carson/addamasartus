document.addEventListener('DOMContentLoaded', function() {
    let isDragging = false;
    let currentX;
    let currentY;
    let initialX;
    let initialY;
    let xOffset = 0;
    let yOffset = 0;
    let dragItem = null;

    function dragStart(e) {
        if (e.target.closest('.floating-container-title')) {
            dragItem = e.target.closest('.floating-container');

            if (e.type === 'touchstart') {
                initialX = e.touches[0].clientX - xOffset;
                initialY = e.touches[0].clientY - yOffset;
            } else {
                initialX = e.clientX - xOffset;
                initialY = e.clientY - yOffset;
            }

            if (dragItem) {
                isDragging = true;
                dragItem.style.cursor = 'grabbing';
            }
        }
    }

    function drag(e) {
        if (isDragging) {
            e.preventDefault();

            if (e.type === 'touchmove') {
                currentX = e.touches[0].clientX - initialX;
                currentY = e.touches[0].clientY - initialY;
            } else {
                currentX = e.clientX - initialX;
                currentY = e.clientY - initialY;
            }

            xOffset = currentX;
            yOffset = currentY;

            setTranslate(currentX, currentY, dragItem);
        }
    }

    function setTranslate(xPos, yPos, el) {
        el.style.transform = `translate3d(${xPos}px, ${yPos}px, 0)`;
    }

    function dragEnd(e) {
        if (dragItem) {
            initialX = currentX;
            initialY = currentY;
            dragItem.style.cursor = '';
            isDragging = false;
            dragItem = null;
        }
    }

    document.addEventListener('touchstart', dragStart, false);
    document.addEventListener('touchmove', drag, false);
    document.addEventListener('touchend', dragEnd, false);
    document.addEventListener('mousedown', dragStart, false);
    document.addEventListener('mousemove', drag, false);
    document.addEventListener('mouseup', dragEnd, false);
});
