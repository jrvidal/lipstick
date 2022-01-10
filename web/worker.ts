import initialize, * as wasm from '../wasm/pkg';
import { CompilationError } from './model';

const compiled = initialize();

self.addEventListener('message', async (event) => {
  const source: string = event.data.source;
  const id: number = event.data.id;

  await compiled;

  wasm.compile(
    source,
    (error: CompilationError[] | undefined, result: string | undefined) => {
      self.postMessage({ error, result, source, id });
    }
  );
});
