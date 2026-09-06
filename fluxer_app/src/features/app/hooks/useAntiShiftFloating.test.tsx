// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAntiShiftFloating} from '@app/features/app/hooks/useAntiShiftFloating';
import {act, createElement, useLayoutEffect, useState} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

let host: HTMLDivElement;
let root: Root;
let target: HTMLDivElement;

function PortalLikeFloating({onReady}: {onReady: (isReady: boolean) => void}) {
	const {setFloating, state} = useAntiShiftFloating(target, true, {placement: 'top'});
	const [mounted, setMounted] = useState(false);
	useLayoutEffect(() => {
		setMounted(true);
	}, []);
	onReady(state.isReady);
	return mounted ? createElement('div', {ref: setFloating, 'data-testid': 'floating'}) : null;
}

async function settle(): Promise<void> {
	for (let attempt = 0; attempt < 20; attempt += 1) {
		await act(async () => {
			await new Promise((resolve) => requestAnimationFrame(() => resolve(null)));
			await Promise.resolve();
		});
	}
}

beforeEach(() => {
	host = document.createElement('div');
	target = document.createElement('div');
	document.body.append(host, target);
	root = createRoot(host);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	document.body.replaceChildren();
});

describe('useAntiShiftFloating', () => {
	it('positions a floating element that mounts after the first commit', async () => {
		const readyStates: Array<boolean> = [];
		act(() => {
			root.render(createElement(PortalLikeFloating, {onReady: (isReady) => readyStates.push(isReady)}));
		});
		await settle();
		expect(readyStates.at(-1)).toBe(true);
		expect(host.querySelector<HTMLElement>('[data-testid="floating"]')?.style.visibility).toBe('visible');
	});
});
