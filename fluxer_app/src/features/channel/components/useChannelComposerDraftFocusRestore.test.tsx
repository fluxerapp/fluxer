// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {act, useRef} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const {canFocusTextareaMock} = vi.hoisted(() => ({canFocusTextareaMock: vi.fn(() => true)}));

vi.mock('@app/features/platform/utils/InputFocusManager', () => ({
	canFocusTextarea: canFocusTextareaMock,
}));

const {useChannelComposerDraftFocusRestore} = await import(
	'@app/features/channel/components/useChannelComposerDraftFocusRestore'
);

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

let container: HTMLDivElement;
let root: Root;
let focusSpy: ReturnType<typeof vi.fn>;

interface HarnessProps {
	initialDraft: string;
	textareaInputDisabled?: boolean;
	inlineEditActive?: boolean;
}

function Harness({initialDraft, textareaInputDisabled = false, inlineEditActive = false}: HarnessProps) {
	const handleRef = useRef<ComposerHandle | null>(null);
	handleRef.current = {focus: focusSpy} as unknown as ComposerHandle;
	const editableRef = useRef<HTMLDivElement | null>(null);
	useChannelComposerDraftFocusRestore({
		handleRef,
		editableRef,
		initialDraft,
		textareaInputDisabled,
		inlineEditActive,
	});
	return (
		<div
			ref={editableRef}
			contentEditable
			suppressContentEditableWarning
			data-flx="channel.use-channel-composer-draft-focus-restore-test.harness.div"
		/>
	);
}

function render(props: HarnessProps): void {
	act(() => {
		root.render(<Harness data-flx="channel.use-channel-composer-draft-focus-restore-test.harness" {...props} />);
	});
}

beforeEach(() => {
	focusSpy = vi.fn();
	canFocusTextareaMock.mockReturnValue(true);
	container = document.createElement('div');
	document.body.append(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	container.remove();
	canFocusTextareaMock.mockReset();
});

describe('useChannelComposerDraftFocusRestore', () => {
	test('focuses the composer when the channel is entered with a pending draft', () => {
		render({initialDraft: 'half written'});
		expect(focusSpy).toHaveBeenCalledTimes(1);
	});

	test('leaves focus alone when there is no pending draft', () => {
		render({initialDraft: ''});
		expect(focusSpy).not.toHaveBeenCalled();
	});

	test('yields to an active inline message edit', () => {
		render({initialDraft: 'half written', inlineEditActive: true});
		expect(focusSpy).not.toHaveBeenCalled();
	});

	test('does nothing when composer input is disabled', () => {
		render({initialDraft: 'half written', textareaInputDisabled: true});
		expect(focusSpy).not.toHaveBeenCalled();
	});

	test('respects the shared focus guard that blocks mobile, modals and popouts', () => {
		canFocusTextareaMock.mockReturnValue(false);
		render({initialDraft: 'half written'});
		expect(focusSpy).not.toHaveBeenCalled();
	});

	test('restores focus only on the channel mount, not on every re-render', () => {
		render({initialDraft: 'half written'});
		render({initialDraft: 'half written'});
		render({initialDraft: 'half written more'});
		expect(focusSpy).toHaveBeenCalledTimes(1);
	});
});
