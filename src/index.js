import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import hljs from 'highlightjs';

window.hljs = hljs;

const app = Main.embed(document.getElementById('root'));

app.ports.log.subscribe(function(str) {
  console.log(str);
});

registerServiceWorker();
