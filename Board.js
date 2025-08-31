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
        const socket = new WebSocket('ws://localhost:12345/ws');
        socket.onopen = () => {
            console.log('WebSocket connection established');
            console.log('board state:', Array.from(cells).map(cell => cell.textContent));
                  
        };
        document.addEventListener('click', (event) => {
            if (event.target.className === 'cell') {
                const index = Array.from(cells).indexOf(event.target);
                if (cells[index].textContent === '') {
                    makeMove(index);
                    socket.send(JSON.stringify({ type: 'move', index }));
                    board = Array.from(cells).map(cell => cell.textContent);
                    console.log('board state:', board);
                    const data = JSON.parse(board);
                    // Process the data as needed
                }
            }
        });
        socket.onmessage = (event) => {
            
        };
    }
});
