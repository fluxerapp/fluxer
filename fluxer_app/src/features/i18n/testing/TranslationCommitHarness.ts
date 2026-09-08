// SPDX-License-Identifier: AGPL-3.0-or-later

import {findTranslationUnsafeTextNodes} from '@app/features/i18n/testing/TranslatorSimulation';
import type {ReactNode} from 'react';
import {flushSync} from 'react-dom';
import {createRoot} from 'react-dom/client';

export interface TranslationCommitOptions {
	before: ReactNode;
	after: ReactNode;
	pipeline: (root: Element) => void;
	betweenCommits?: () => void;
	inspect?: (host: HTMLElement) => void;
}

export interface TranslationCommitResult {
	error: Error | null;
	htmlBeforeTranslation: string;
	htmlAfterTranslation: string;
	htmlAfterCommit: string;
	unsafeTextNodesBeforeTranslation: number;
}

export function commitUnderTranslation({
	before,
	after,
	pipeline,
	betweenCommits,
	inspect,
}: TranslationCommitOptions): TranslationCommitResult {
	const host = document.createElement('div');
	document.body.appendChild(host);
	let error: Error | null = null;
	const root = createRoot(host, {
		onUncaughtError: (caught) => {
			error ??= caught as Error;
		},
		onCaughtError: (caught) => {
			error ??= caught as Error;
		},
		onRecoverableError: () => {},
	});
	flushSync(() => root.render(before));
	const htmlBeforeTranslation = host.innerHTML;
	const unsafeTextNodesBeforeTranslation = findTranslationUnsafeTextNodes(host).length;
	pipeline(host);
	const htmlAfterTranslation = host.innerHTML;
	try {
		betweenCommits?.();
	} catch (caught) {
		error ??= caught as Error;
	}
	try {
		flushSync(() => root.render(after));
	} catch (caught) {
		error ??= caught as Error;
	}
	const htmlAfterCommit = host.innerHTML;
	inspect?.(host);
	try {
		root.unmount();
	} catch {}
	host.remove();
	return {error, htmlBeforeTranslation, htmlAfterTranslation, htmlAfterCommit, unsafeTextNodesBeforeTranslation};
}

export function isTranslationDomCrash(error: Error | null): boolean {
	if (error == null) {
		return false;
	}
	return (
		error.message.includes("Failed to execute 'insertBefore' on 'Node'") ||
		error.message.includes("Failed to execute 'removeChild' on 'Node'")
	);
}
