// SPDX-License-Identifier: AGPL-3.0-or-later

import {router} from '@app/app/Router';
import {Routes} from '@app/app/Routes';
import {resolveAuthShellCategory} from '@app/features/app/components/skeleton/AuthShellHint';
import {readHashParam} from '@app/features/app/hooks/useHashParam';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {
	resolveForgotPasswordRedirect,
	resolveMissingResetTokenRedirect,
} from '@app/features/auth/flow/AccountRecoveryRedirects';
import {warmAuthRuntimeTarget} from '@app/features/auth/flow/AuthRuntimeTargetBoundary';
import {authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';

const FIRST_SCREEN_BUDGET_MS = 3000;

const logger = new Logger('AuthFirstScreen');

export function resolveSignedOutFirstScreenPath(pathname: string, search: string): string {
	if (pathname !== Routes.LOGIN && resolveAuthShellCategory(pathname) === 'login') {
		return Routes.LOGIN;
	}
	return `${pathname}${search}`;
}

async function prepareRuntimeDependentFirstScreen(runtime: RuntimeConfigSnapshot | null): Promise<void> {
	if (runtime === null) {
		return;
	}
	const {pathname, hash} = window.location;
	if (pathname === Routes.FORGOT_PASSWORD) {
		const redirect = resolveForgotPasswordRedirect(runtime);
		if (redirect !== null) {
			await router.preload(redirect);
		}
		return;
	}
	if (pathname !== Routes.RESET_PASSWORD) {
		return;
	}
	const token = readHashParam(hash, 'token');
	if (token === null) {
		await router.preload(resolveMissingResetTokenRedirect(runtime));
		return;
	}
	const {warmResetPasswordTokenCheck} = await loadLazyModule(
		() => import('@app/features/auth/state/ResetPasswordTokenCheck'),
	);
	await warmResetPasswordTokenCheck(token, authRequestTargetFromSnapshot(runtime));
}

export async function prepareSignedOutFirstScreen(): Promise<void> {
	const signal = AbortSignal.timeout(FIRST_SCREEN_BUDGET_MS);
	const path = resolveSignedOutFirstScreenPath(window.location.pathname, window.location.search);
	const work = Promise.allSettled([
		router.preload(path),
		warmAuthRuntimeTarget(signal).then(prepareRuntimeDependentFirstScreen),
	]).then((results) => {
		for (const result of results) {
			if (result.status === 'rejected' && !signal.aborted) {
				logger.warn('Failed to prepare the signed-out first screen', result.reason);
			}
		}
	});
	let budget: ReturnType<typeof setTimeout> | null = null;
	const expired = new Promise<void>((resolve) => {
		budget = setTimeout(resolve, FIRST_SCREEN_BUDGET_MS);
	});
	try {
		await Promise.race([work, expired]);
	} finally {
		if (budget !== null) {
			clearTimeout(budget);
		}
	}
}
