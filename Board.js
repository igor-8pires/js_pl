document.addEventListener('DOMContentLoaded', () => {
    const urlParams = new URLSearchParams(window.location.search);
    const size = parseInt(urlParams.get('size'));
    const mode = urlParams.get('mode');

  
    document.getElementById('board').style.gridTemplateColumns = `repeat(${size}, 100px)`;
    document.getElementById('title').textContent = `Tic Tac Toe - ${size}x${size}`;

    let currentPlayer = 'X';
    let cells = document.getElementsByClassName('cell');

    for (let i = 0; i < size * size; i++) {
        const cell = document.createElement('div');
        cell.className = 'cell';
        document.getElementById('board').appendChild(cell);
    }
    function makeMove(index) {
        console.log('Cell clicked:', index);
        console.log('Current player:', currentPlayer);
        if(currentPlayer === 'X') {
            if (cells[index].textContent === '') {
                cells[index].textContent = currentPlayer;
                currentPlayer = currentPlayer === 'X' ? 'O' : 'X';
                console.log('JSON:', JSON.stringify(Array.from(cells).map(cell => cell.textContent)));
            }
        }
        console.log('JSON:', JSON.stringify(Array.from(cells).map(cell => cell.textContent)));

    }
    function makeO(index) {
    cells[index].textContent = 'O';
    currentPlayer = 'X';
    console.log('JSON:', JSON.stringify(Array.from(cells).map(cell => cell.textContent)));

    }

    if (mode === 'computer') {
        const socket = new WebSocket('ws://localhost:12345/ws');
        socket.addEventListener('open', (event) => {
            console.log('WebSocket connection established');


        });
        for (const cell of cells) {
            cell.onclick = () => {
                let boardState = Array.from(cells).map(cell => cell.textContent);
                if (cell.classList.contains('cell') && cell.textContent === '') {
                    const index = Array.from(cells).indexOf(cell) + 1;
                    socket.send(JSON.stringify({ 'board': boardState, 'index': index }));
                    makeMove(index - 1);
                }
            };
        }

        socket.addEventListener('message', (event) => {
            const data = JSON.parse(event.data);
            let tab= data.board;
            console.log(tab);
            for (let i = 0; i < tab.length; i++) {
                if(tab[i] == 'x' && tab[i] == 'o'){
                    cells[i].textContent = tab;
                } else {
                    cells[i].textContent = "";
                }
            }
            console.log('Received:', data);
            Index=data.index - 1;
            makeO(Index);

        });
     
    }
});
