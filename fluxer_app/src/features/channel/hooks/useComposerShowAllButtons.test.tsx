// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useComposerShowAllButtons} from '@app/features/channel/hooks/useComposerShowAllButtons';
import {act, useRef} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

let root: Root | null = null;
let host: HTMLElement | null = null;
let composerClientWidth = 0;

const Composer: React.FC<{isMobile: boolean}> = ({isMobile}) => {
	const containerRef = useRef<HTMLDivElement>(null);
	const showAllButtons = useComposerShowAllButtons(containerRef, isMobile);
	return (
		<div
			ref={containerRef}
			data-composer=""
			style={{paddingLeft: '8px', paddingRight: '8px'}}
			data-flx="channel.use-composer-show-all-buttons-test.composer.div"
		>
			{showAllButtons ? 'all' : 'collapsed'}
		</div>
	);
};

function mount(isMobile: boolean): string {
	host = document.createElement('div');
	document.body.appendChild(host);
	root = createRoot(host);
	act(() => {
		root?.render(<Composer isMobile={isMobile} data-flx="channel.use-composer-show-all-buttons-test.mount.composer" />);
	});
	return host.textContent ?? '';
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	vi.spyOn(HTMLElement.prototype, 'clientWidth', 'get').mockImplementation(function (this: HTMLElement) {
		return this.hasAttribute('data-composer') ? composerClientWidth : 0;
	});
	vi.spyOn(window, 'requestAnimationFrame').mockImplementation(() => 1);
});

afterEach(() => {
	act(() => {
		root?.unmount();
	});
	host?.remove();
	root = null;
	host = null;
	vi.restoreAllMocks();
});

test('a narrow composer collapses its buttons in the first committed frame', () => {
	composerClientWidth = 480;
	expect(mount(false)).toBe('collapsed');
});

test('the threshold applies to the content box, matching the resize observer', () => {
	composerClientWidth = 510;
	expect(mount(false)).toBe('collapsed');
	act(() => {
		root?.unmount();
	});
	composerClientWidth = 517;
	expect(mount(false)).toBe('all');
});

test('a wide composer keeps every button', () => {
	composerClientWidth = 900;
	expect(mount(false)).toBe('all');
});

test('an unmeasured composer keeps the default until the observer reports a width', () => {
	composerClientWidth = 0;
	expect(mount(false)).toBe('all');
});

test('the mobile layout always shows every button', () => {
	composerClientWidth = 320;
	expect(mount(true)).toBe('all');
});
