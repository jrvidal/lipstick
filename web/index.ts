import type { Text } from '@codemirror/text';
import type { Transaction } from '@codemirror/state';
import { parse } from 'ansicolor';
// @ts-ignore
import hljs from 'highlight.js/lib/core.js';
// @ts-ignore
import c from 'highlight.js/lib/languages/c';
import { $, toggleEl } from './utils';

import './modal';
import { CompilationError } from './model';
import { createEditor } from './editor';

const DEBOUNCE = 400;

hljs.registerLanguage('c', c);

const worker = new Worker(new URL('worker.ts', import.meta.url), {
  type: 'module',
});
worker.addEventListener('message', onCompilationResult);

const generateLinkComponent = createGenerateLinkComponent();
const resultsComponent = createResultsComponent((compilationErrorRange) => {
  editor.showCompilationError(compilationErrorRange);
});

const storage = createStorage();

const initialCode = generateLinkComponent.current() ?? storage.retrieve() ?? $('.default-code')?.textContent!;

const onActualUpdate = (() => {
  let timer: number | null = null;

  return function onUpdate(tr: Transaction) {
    generateLinkComponent.reset();
    resultsComponent.clearSelection();

    if (timer != null) {
      clearTimeout(timer);
    }

    timer = setTimeout(onDebouncedUpdate, DEBOUNCE);
  };
})();

const editor = createEditor({
  parent: $('.editor')!,
  onUpdate(tr) {
    if (last === tr.newDoc) {
      return;
    }

    onActualUpdate(tr);
  },
  initialState: initialCode,
});

let processingCounter = 0;
let processing: number | null = -1;
let last: Text = editor.doc();

doCompile(initialCode);

function onDebouncedUpdate() {
  if (last === editor.doc()) {
    return;
  }

  const source = editor.doc().toJSON().join('\n');

  if (source.trim() === '') {
    return;
  }

  last = editor.doc();

  processing = processingCounter++;

  doCompile(source);
}

function doCompile(source: string) {
  worker.postMessage({ source, id: processing });
}

function onCompilationResult(event: MessageEvent<any>): void {
  const msg = event.data as {
    id: number;
    source: string;
  } & (
    | {
        result: string;
        error: undefined;
      }
    | {
        result: undefined;
        error: CompilationError[];
      }
  );

  const { source, id } = msg;

  if (id === processing) {
    processing = null;
  }

  if (msg.result != null) {
    resultsComponent.success(msg.result);
    storage.save(source);
  } else {
    resultsComponent.error(msg.error);
  }
}

export type CompilationRange = {
  startLine: number;
  startColumn: number;
  endLine: number;
  endColumn: number;
};

function createResultsComponent(
  onSelectError: (range: CompilationRange) => void
) {
  const transpiledContainer = $('.transpiled-container')!;
  const transpiled = $('.transpiled')!;

  const errors = $('.errors')!;
  const errorsContainer = $('.errors-container')!;

  errors.addEventListener('click', (event) => {
    let target = event.target as HTMLDivElement;

    while (target !== errors && !target.classList.contains('error')) {
      target = target.parentElement as HTMLDivElement;
    }

    if (!target.classList.contains('error')) {
      return;
    }

    const index = [...errors.childNodes].indexOf(target);

    const compilationError = state.errors[index];

    if (compilationError == null) {
      return;
    }

    state.selected = index;

    onSelectError({
      startLine: compilationError[2],
      startColumn: compilationError[3],
      endLine: compilationError[4],
      endColumn: compilationError[5],
    });

    renderSelected();
  });

  const pool = new Set<HTMLSpanElement>();

  function getFromPool() {
    const value: HTMLSpanElement = pool.values().next()?.value;

    if (value != null) {
      pool.delete(value);
      return value;
    } else {
      return document.createElement('span');
    }
  }

  function errorElement(error: string) {
    const container = document.createElement('div');
    container.classList.add('error');

    const parsed = parse(error);

    for (const { text, css } of parsed.spans) {
      const span = getFromPool();
      span.setAttribute('style', css);
      span.textContent = text;

      container.appendChild(span);
    }

    return container;
  }

  function destroyErrorElement(errorEl: Element) {
    for (const child of errorEl.childNodes) {
      pool.add(child as HTMLSpanElement);
    }
  }

  function renderSelected() {
    let index = 0;

    for (const el of errors.children) {
      if (index === state.selected) {
        el.classList.add('selected-error');
      } else {
        el.classList.remove('selected-error');
      }

      index += 1;
    }
  }

  const state = {
    errors: [] as CompilationError[],
    selected: null as number | null,
  };

  return {
    success(transpiledSource: string) {
      toggleEl(transpiledContainer, true);
      toggleEl(errorsContainer, false);
      state.errors = [];

      transpiled.textContent = transpiledSource;
      hljs.highlightElement(transpiled);
    },

    error(compilationErrors: CompilationError[]) {
      toggleEl(transpiledContainer, false);
      toggleEl(errorsContainer, true);
      state.errors = compilationErrors;

      setTimeout(() => {
        if (state.errors !== compilationErrors) {
          return;
        }

        while (errors.firstChild != null) {
          destroyErrorElement(errors.firstChild as Element);
          errors.removeChild(errors.firstChild);
        }

        for (const error of compilationErrors) {
          errors.appendChild(errorElement(error[0]));
        }
      });
    },

    clearSelection() {
      state.selected = null;
      renderSelected();
    },
  };
}

function createStorage() {
  return {
    retrieve() {
      const code = localStorage.getItem('code');
      return code != null ? code : undefined;
    },

    save(code: string) {
      localStorage.setItem('code', code);
    },
  };
}

function createGenerateLinkComponent() {
  const button = $('.generate-link')!;
  const link: HTMLAnchorElement = $('.code-link')!;

  function create(code: string) {
    const url = new URL(window.location.href);

    url.searchParams.set('code', code);

    toggleEl(link, true);
    toggleEl(button, false);
    link.href = url.toString();
  }

  button.addEventListener('click', () => {
    const code = editor.doc().toJSON().join('\n');
    create(code);
  });

  return {
    reset() {
      toggleEl(button, true);
      toggleEl(link, false);
    },
    current() {
      const code = new URL(location.href).searchParams.get('code');
      return code != null ? code : undefined;
    },
  };
}

if (process.env.NODE_ENV === 'production' && process.env.ANALYTICS_URL) {
  fetch(process.env.ANALYTICS_URL);
}
