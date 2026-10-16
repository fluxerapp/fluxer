// SPDX-License-Identifier: AGPL-3.0-or-later

import {useHashParam} from '@app/features/app/hooks/useHashParam';
import {VerificationResult} from '@app/features/auth/commands/AuthenticationCommands';
import {useAuthSingleUseRequest} from '@app/features/auth/flow/AuthSingleUseRequest';
import {
	createVerificationError,
	type VerificationError,
	VerificationErrorType,
} from '@app/features/auth/types/VerificationError';
import {useState} from 'react';

export interface AuthTokenVerificationState {
	readonly isLoading: boolean;
	readonly isSuccess: boolean;
	readonly error: VerificationError | null;
}

export function useAuthTokenVerification(
	verify: (token: string) => Promise<VerificationResult>,
): AuthTokenVerificationState {
	const [isLoading, setIsLoading] = useState(true);
	const [isSuccess, setIsSuccess] = useState(false);
	const [error, setError] = useState<VerificationError | null>(null);
	const token = useHashParam('token');
	useAuthSingleUseRequest(token ?? '', async (request) => {
		if (token == null || token.length === 0) {
			setError(createVerificationError(VerificationErrorType.INVALID_TOKEN));
			setIsLoading(false);
			return;
		}
		const result = await verify(token);
		if (!request.isCurrent()) {
			return;
		}
		switch (result) {
			case VerificationResult.SUCCESS:
				setIsSuccess(true);
				break;
			case VerificationResult.EXPIRED_TOKEN:
				setError(createVerificationError(VerificationErrorType.LINK_EXPIRED));
				break;
			case VerificationResult.SERVER_ERROR:
				setError(createVerificationError(VerificationErrorType.SERVER_ERROR));
				break;
		}
		setIsLoading(false);
	});
	return {isLoading, isSuccess, error};
}
