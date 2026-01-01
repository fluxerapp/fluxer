/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useCallback, useMemo, useState} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';

export type DesktopHandoffMode = 'idle' | 'selecting' | 'login' | 'generating' | 'displaying' | 'error';

type Options = {
	enabled: boolean;
	hasStoredAccounts: boolean;

	initialMode?: DesktopHandoffMode;
};

export function useDesktopHandoffFlow({enabled, hasStoredAccounts, initialMode}: Options) {
	const derivedInitial = useMemo<DesktopHandoffMode>(() => {
		if (!enabled) return 'idle';
		if (initialMode) return initialMode;
		return hasStoredAccounts ? 'selecting' : 'login';
	}, [enabled, hasStoredAccounts, initialMode]);

	const [mode, setMode] = useState<DesktopHandoffMode>(derivedInitial);
	const [code, setCode] = useState<string | null>(null);
	const [error, setError] = useState<string | null>(null);

	const start = useCallback(
		async ({token, userId}: {token: string; userId: string}) => {
			if (!enabled) return;

			setMode('generating');
			setError(null);
			setCode(null);

			try {
				const result = await AuthenticationActionCreators.initiateDesktopHandoff();
				await AuthenticationActionCreators.completeDesktopHandoff({
					code: result.code,
					token,
					userId,
				});

				setCode(result.code);
				setMode('displaying');
			} catch (e) {
				setMode('error');
				setError(e instanceof Error ? e.message : String(e));
			}
		},
		[enabled],
	);

	const switchToLogin = useCallback(() => {
		setMode('login');
		setError(null);
	}, []);

	const retry = useCallback(() => {
		setError(null);
		setCode(null);
		setMode(hasStoredAccounts ? 'selecting' : 'login');
	}, [hasStoredAccounts]);

	return {
		mode,
		code,
		error,

		setMode,

		start,
		switchToLogin,
		retry,
	};
}
