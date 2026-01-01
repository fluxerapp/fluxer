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
import {CheckCircleIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useId, useState} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import FormField from '~/components/auth/FormField';
import {Button} from '~/components/uikit/Button/Button';
import {useAuthForm} from '~/hooks/useAuthForm';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {Routes} from '~/Routes';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ConnectionStore from '~/stores/ConnectionStore';
import UserStore from '~/stores/UserStore';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from './PendingVerificationPage.module.css';

const PendingVerificationPageContent = observer(function PendingVerificationPageContent() {
	const {t} = useLingui();
	const betaCodeId = useId();
	const [redeemSuccess, setRedeemSuccess] = useState(false);
	const token = AuthenticationStore.token;
	const socket = ConnectionStore.socket;
	const currentUser = UserStore.currentUser;

	useFluxerDocumentTitle(t`Pending Verification`);

	useEffect(() => {
		if (token && !socket) {
			AuthenticationActionCreators.startSession(token, {startGateway: true});
		}
	}, [token, socket]);

	useEffect(() => {
		if (currentUser && currentUser.pendingManualVerification === false) {
			RouterUtils.replaceWith(Routes.ME);
		}
	}, [currentUser]);

	const {form, isLoading, fieldErrors} = useAuthForm({
		initialValues: {
			betaCode: '',
		},
		onSubmit: async (values) => {
			if (!values.betaCode) {
				throw new Error('Beta code is required');
			}

			await AuthenticationActionCreators.redeemBetaCode(values.betaCode);
			setRedeemSuccess(true);

			setTimeout(() => {
				RouterUtils.transitionTo(Routes.ME);
			}, 1500);
		},
	});

	if (redeemSuccess) {
		return (
			<div className={styles.header}>
				<div className={clsx(styles.iconWrapper, styles.iconWrapperSuccess)}>
					<CheckCircleIcon className={styles.iconSuccess} />
				</div>
				<h1 className={styles.title}>
					<Trans>Beta Code Redeemed!</Trans>
				</h1>
				<p className={styles.description}>
					<Trans>Your account has been approved. Redirecting you to the app...</Trans>
				</p>
			</div>
		);
	}

	return (
		<>
			<div className={styles.header}>
				<div className={clsx(styles.iconWrapper, styles.iconWrapperWarning)}>
					<WarningCircleIcon className={styles.iconWarning} />
				</div>
				<h1 className={styles.title}>
					<Trans>Registration pending manual review</Trans>
				</h1>
				<p className={styles.description}>
					<Trans>
						To manage the influx of users during our public beta, we are throttling registrations and will manually
						review new registration attempts at the rate we can manage this.
					</Trans>
				</p>
				<p className={styles.description}>
					<Trans>
						Your account has been created, but access is temporarily limited until approval. You will be notified once
						your account is approved.
					</Trans>
				</p>
			</div>

			<div className={styles.section}>
				<h2 className={styles.sectionTitle}>
					<Trans>Got Code?</Trans>
				</h2>
				<p className={styles.sectionDescription}>
					<Trans>If you have received a beta code, you can enter it below to get immediate access.</Trans>
				</p>

				<form className={styles.form} onSubmit={form.handleSubmit}>
					<FormField
						id={betaCodeId}
						name="betaCode"
						type="text"
						required
						label={t`Beta code`}
						value={form.getValue('betaCode')}
						onChange={(value) => form.setValue('betaCode', value)}
						error={form.getError('betaCode') || fieldErrors?.beta_code}
					/>

					<Button type="submit" fitContainer disabled={isLoading || form.isSubmitting}>
						<Trans>Redeem Beta Code</Trans>
					</Button>
				</form>
			</div>

			<div className={styles.footer}>
				<Button
					variant="secondary"
					fitContainer
					onClick={() => {
						AuthenticationActionCreators.logout();
						RouterUtils.transitionTo('/login');
					}}
				>
					<Trans>Log Out</Trans>
				</Button>
			</div>
		</>
	);
});

const PendingVerificationPage = observer(function PendingVerificationPage() {
	return <PendingVerificationPageContent />;
});

export default PendingVerificationPage;
