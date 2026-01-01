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
import * as MfaActionCreators from '~/actions/MfaActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import styles from '~/components/modals/MfaTotpDisableModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';

interface FormInputs {
	code: string;
}

export const MfaTotpDisableModal = observer(() => {
	const {t} = useLingui();
	const form = useForm<FormInputs>();

	const onSubmit = async (data: FormInputs) => {
		await MfaActionCreators.disableMfaTotp(data.code.split(' ').join(''));
		ModalActionCreators.pop();
		ToastActionCreators.createToast({type: 'success', children: <Trans>Two-factor authentication disabled</Trans>});
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Disable two-factor authentication form`}>
				<Modal.Header title={t`Remove authenticator app`} />
				<Modal.Content className={confirmStyles.content}>
					<Input
						{...form.register('code')}
						autoFocus={true}
						autoComplete="one-time-code"
						error={form.formState.errors.code?.message}
						label={t`Code`}
						required={true}
						footer={
							<p className={styles.footer}>
								<Trans>Enter the 6-digit code from your authenticator app.</Trans>
							</p>
						}
					/>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={form.formState.isSubmitting}>
						<Trans>Continue</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
