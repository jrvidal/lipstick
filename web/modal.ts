import { $, toggleEl } from './utils';

const modal: HTMLDivElement = $('.modal')!;
const overlay = $('.overlay')!;

let showing = true;

$('.help')!.addEventListener('click', () => {
  toggleEl(modal, true);
  toggleEl(overlay, true);
  overlay.classList.remove('hidden-for-real');
});

const dismissModal = () => {
  showing = false;
  toggleEl(modal, false);
  toggleEl(overlay, false);
};

overlay.addEventListener('transitionend', (ev) => {
  if (!showing) {
    overlay.classList.add('hidden-for-real');
    showing = true;
  }
});

overlay.addEventListener('click', dismissModal);

$('.dismiss')!.addEventListener('click', dismissModal);
