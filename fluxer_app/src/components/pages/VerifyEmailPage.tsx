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

import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import {VerificationResult} from '~/actions/AuthenticationActionCreators';
import {AuthRouterLink} from '~/components/auth/AuthRouterLink';
import {Spinner} from '~/components/uikit/Spinner';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {useHashParam} from '~/hooks/useHashParam';
import {createVerificationError, type VerificationError, VerificationErrorType} from '~/types/VerificationError';
import styles from './VerifyEmailPage.module.css';

const renderErrorMessage = (error: VerificationError | null) => {
	if (!error) return null;

	switch (error.type) {
		case VerificationErrorType.LINK_EXPIRED:
			return <Trans>Link expired. Your email was likely already verified.</Trans>;
		default:
			return (
				<Trans>
					An unexpected server error occurred. Please try reloading the page or requesting a new verification email.
				</Trans>
			);
	}
};

const VerifyPage = observer(function VerifyPage() {
	const {t} = useLingui();
	const [isLoading, setIsLoading] = useState(true);
	const [isSuccess, setIsSuccess] = useState(false);
	const [error, setError] = useState<VerificationError | null>(null);

	useFluxerDocumentTitle(t`Verify Email`);

	const token = useHashParam('token');

	useEffect(() => {
		const performVerification = async () => {
			if (!token) {
				setError(createVerificationError(VerificationErrorType.INVALID_TOKEN));
				setIsLoading(false);
				return;
			}

			const result = await AuthenticationActionCreators.verifyEmail(token);

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
		};

		performVerification();
	}, [token]);

	if (isLoading) {
		return (
			<div className={styles.container}>
				<div className={styles.iconContainer}>
					<div className={styles.spinnerWrapper}>
						<Spinner />
					</div>
				</div>
				<div className={styles.loadingPlaceholder} />
				<div className={styles.descriptionPlaceholder} />
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<div className={styles.iconContainer}>
				<div className={clsx(styles.iconCircle, isSuccess ? styles.iconCircleSuccess : styles.iconCircleError)}>
					{isSuccess ? (
						<CheckIcon className={styles.icon} weight="bold" />
					) : (
						<XIcon className={styles.icon} weight="bold" />
					)}
				</div>
			</div>

			{isSuccess ? (
				<>
					<h1 className={styles.title}>
						<Trans>Email verified successfully</Trans>
					</h1>
					<p className={styles.description}>
						<Trans>Your email has been verified. You can now log in to your account.</Trans>
					</p>
					<div className={styles.footer}>
						<AuthRouterLink to="/login" className={styles.link}>
							<Trans>Go to login</Trans>
						</AuthRouterLink>
					</div>
				</>
			) : (
				<>
					<h1 className={styles.title}>
						<Trans>Verification failed</Trans>
					</h1>
					<p className={styles.description}>{renderErrorMessage(error)}</p>
					<div className={styles.footer}>
						<div>
							<AuthRouterLink to="/login" className={styles.link}>
								<Trans>Go to login</Trans>
							</AuthRouterLink>
						</div>
						<div>
							<AuthRouterLink to="/register" className={styles.secondaryLink}>
								<Trans>Create new account</Trans>
							</AuthRouterLink>
						</div>
					</div>
				</>
			)}
		</div>
	);
});

export default VerifyPage;
