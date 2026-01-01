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
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import styles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';

interface FormInputs {
	new_password: string;
	confirm_password: string;
}

export const PasswordChangeModal = observer(() => {
	const {t} = useLingui();
	const form = useForm<FormInputs>();

	const onSubmit = async (data: FormInputs) => {
		if (data.new_password !== data.confirm_password) {
			form.setError('confirm_password', {message: t`Passwords do not match`});
			return;
		}

		await UserActionCreators.update({new_password: data.new_password});
		ModalActionCreators.pop();
		ToastActionCreators.createToast({type: 'success', children: <Trans>Password changed</Trans>});
	};

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'new_password',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit}>
				<Modal.Header title={t`Update your password`} />
				<Modal.Content className={styles.content}>
					<p className={styles.descriptionText}>
						<Trans>Enter your new password.</Trans>
					</p>

					<div className={styles.inputContainer}>
						<Input
							{...form.register('new_password')}
							autoFocus={true}
							autoComplete="new-password"
							error={form.formState.errors.new_password?.message}
							label={t`New password`}
							maxLength={128}
							minLength={8}
							placeholder={'•'.repeat(32)}
							required={true}
							type="password"
						/>
						<Input
							{...form.register('confirm_password')}
							autoComplete="new-password"
							error={form.formState.errors.confirm_password?.message}
							label={t`Confirm new password`}
							maxLength={128}
							minLength={8}
							placeholder={'•'.repeat(32)}
							required={true}
							type="password"
						/>
					</div>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting}>
						<Trans>Continue</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
