document.addEventListener('DOMContentLoaded', () => {
    const urlParams = new URLSearchParams(window.location.search);
    const size = parseInt(urlParams.get('size'));
    const mode = urlParams.get('mode');

    for (let i = 0; i < size * size; i++) {
        const cell = document.createElement('div');
        cell.className = 'cell';
        cell.onclick = () => makeMove(i);
        document.getElementById('board').appendChild(cell);
    }
    document.getElementById('board').style.gridTemplateColumns = `repeat(${size}, 100px)`;
    document.getElementById('title').textContent = `Tic Tac Toe - ${size}x${size}`;

    let currentPlayer = 'X';
    let cells = document.getElementsByClassName('cell');

    function makeMove(index) {
        if (cells[index].textContent === '') {
            cells[index].textContent = currentPlayer;
            currentPlayer = currentPlayer === 'X' ? 'O' : 'X';
        }
    }
    if (mode === 'computer') {
        // consulting prolog AI on websocket
        const socket = new WebSocket('ws://localhost:12345');
        socket.onopen = () => {
            console.log('WebSocket connection established');
        };
        socket.onmessage = (event) => {
            const data = JSON.parse(event.data);
            if (data.type === 'move') {
                console.log('move made from player:', data);
                //const { index } = data;
                //makeMove(index);
            }
        };
    }
});
