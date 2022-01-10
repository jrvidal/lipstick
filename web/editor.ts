import { basicSetup, EditorState, EditorView } from '@codemirror/basic-setup';
import { StateEffect, StateField, Transaction } from '@codemirror/state';
import { Decoration, DecorationSet } from '@codemirror/view';
import { Range } from '@codemirror/rangeset';
import { rust } from '@codemirror/lang-rust';
import { CompilationRange } from './index';

type Underline = {
  from: number;
  to: number;
} | null;

const underlineErrorEffect = StateEffect.define<Underline>();
const underlineMark = Decoration.mark({ class: 'cm-underline' });
const underlineField = StateField.define<DecorationSet>({
  create() {
    return Decoration.none;
  },
  update(underlines, tr) {
    underlines = underlines.map(tr.changes);

    for (const effect of tr.effects) {
      if (effect.is(underlineErrorEffect)) {
        const spec = {
          filter() {
            return false;
          },
          add: [] as Range<Decoration>[],
        };

        if (effect.value != null) {
          spec.add = [underlineMark.range(effect.value.from, effect.value.to)];
        }

        underlines = underlines.update(spec);
      }
    }

    return underlines;
  },
  provide: (f) => EditorView.decorations.from(f),
});

export type EditorProps = {
  parent: HTMLElement;
  onUpdate: (tr: Transaction) => void;
  initialState: string;
};

export function createEditor({ parent, onUpdate, initialState }: EditorProps) {
  const state = EditorState.create({
    doc: initialState,
    extensions: [basicSetup, rust()],
  });

  const view = new EditorView({
    state,
    parent,
    dispatch(tr) {
      onUpdate(tr);

      if (tr.effects.length === 0 && tr.docChanged) {
        view.update([
          tr,
          tr.state.update({ effects: [underlineErrorEffect.of(null)] }),
        ]);
      } else {
        view.update([tr]);
      }
    },
  });

  if (process.env.NODE_ENV === 'development') {
    (window as any).view = view;
  }

  return {
    showCompilationError(range: CompilationRange) {
      const from =
        view.state.doc.line(range.startLine).from + range.startColumn;
      const to = view.state.doc.line(range.endLine).from + range.endColumn;

      const effects: StateEffect<any>[] = [
        underlineErrorEffect.of({ from, to }),
        EditorView.scrollIntoView(from, {
          y: 'start',
        }),
      ];

      if (!view.state.field(underlineField, false)) {
        effects.push(StateEffect.appendConfig.of([underlineField]));
      }

      view.dispatch({
        effects,
      });
    },
    doc() {
      return view.state.doc;
    },
  };
}
