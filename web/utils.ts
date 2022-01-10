const HIDDEN_CLASS = 'hidden';

export const $ = document.querySelector.bind(document);

export function toggleEl(el: Element, show: boolean) {
  const classList = el.classList;

  if (show) {
    classList.remove(HIDDEN_CLASS);
  } else {
    classList.add(HIDDEN_CLASS);
  }
}
