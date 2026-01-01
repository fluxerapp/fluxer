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
import {observer} from 'mobx-react-lite';
import {useEffect, useId} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import {AuthRouterLink} from '~/components/auth/AuthRouterLink';
import FormField from '~/components/auth/FormField';
import {Button} from '~/components/uikit/Button/Button';
import {useAuthForm} from '~/hooks/useAuthForm';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {useHashParam} from '~/hooks/useHashParam';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from './ResetPasswordPage.module.css';

const ResetPasswordPage = observer(function ResetPasswordPage() {
	const {t} = useLingui();
	const passwordId = useId();
	const confirmPasswordId = useId();

	useFluxerDocumentTitle(t`Reset Password`);

	const token = useHashParam('token');

	const {form, isLoading, fieldErrors} = useAuthForm({
		initialValues: {
			password: '',
			confirmPassword: '',
		},
		onSubmit: async (values) => {
			if (!token) {
				form.setError('password', 'Invalid or missing reset token');
				return;
			}

			if (values.password !== values.confirmPassword) {
				form.setError('confirmPassword', 'Passwords do not match');
				return;
			}

			const response = await AuthenticationActionCreators.resetPassword(token, values.password);
			await AuthenticationActionCreators.completeLogin({
				token: response.token,
				userId: response.user_id,
			});
		},
		firstFieldName: 'password',
	});

	useEffect(() => {
		if (!token) {
			RouterUtils.replaceWith('/forgot');
		}
	}, [token]);

	return (
		<>
			<h1 className={styles.title}>
				<Trans>Set new password</Trans>
			</h1>

			<p className={styles.description}>
				<Trans>Enter your new password below to complete the reset process.</Trans>
			</p>

			<form className={styles.form} onSubmit={form.handleSubmit}>
				<FormField
					id={passwordId}
					name="password"
					type="password"
					autoComplete="new-password"
					required
					label={t`New password`}
					value={form.getValue('password')}
					onChange={(value) => form.setValue('password', value)}
					error={form.getError('password') || fieldErrors?.password}
				/>

				<FormField
					id={confirmPasswordId}
					name="confirmPassword"
					type="password"
					autoComplete="new-password"
					required
					label={t`Confirm new password`}
					value={form.getValue('confirmPassword')}
					onChange={(value) => form.setValue('confirmPassword', value)}
					error={form.getError('confirmPassword')}
				/>

				<Button type="submit" fitContainer disabled={isLoading || form.isSubmitting}>
					<Trans>Reset password</Trans>
				</Button>
			</form>

			<div className={styles.footer}>
				<AuthRouterLink to="/login" className={styles.link}>
					<Trans>Back to login</Trans>
				</AuthRouterLink>
			</div>
		</>
	);
});

export default ResetPasswordPage;
