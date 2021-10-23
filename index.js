import {unref} from "./target/scala-3.0.2/unref-opt/main.js";

const inputElement = document.querySelector('#input');
const unrefElement = document.querySelector('#unref');
const resultElement = document.querySelector('#result');

unrefElement.addEventListener('click', () => {
  try {
    resultElement.textContent = `/${unref(inputElement.value)}/`;
  } catch (err) {
    resultElement.textContent = err.message;
  }
});