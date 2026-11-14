// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {useDebouncedAsyncResource} from '@app/features/platform/hooks/useDebouncedAsyncResource';
import {isAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useMemo} from 'react';

type UsernameAvailability = 'idle' | 'checking' | 'available' | 'taken';

const logger = new Logger('useUsernameAvailability');
const CHECK_DELAY_MS = 350;

export function useUsernameAvailability(
	target: InstanceHTTPTarget,
	username: string,
	enabled: boolean,
): UsernameAvailability {
	const request = useMemo(() => {
		if (!enabled || username.length === 0) {
			return null;
		}
		return {
			input: username,
			load: (name: string, signal: AbortSignal) =>
				AuthenticationCommands.checkUsernameAvailability(name, target, signal),
		};
	}, [enabled, target, username]);
	const result = useDebouncedAsyncResource(request, CHECK_DELAY_MS, (error, name) => {
		if (!(error instanceof HttpError) && !isAccountTransitionAbortError(error)) {
			logger.warn('Failed to check username availability', {username: name, error});
		}
	});
	if (!enabled || username.length === 0 || result?.input !== username) {
		return 'idle';
	}
	if (result.status === 'pending') {
		return 'checking';
	}
	if (result.status === 'ready') {
		return result.value ? 'available' : 'taken';
	}
	return 'idle';
}
