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
import * as AuthSessionActionCreators from '~/actions/AuthSessionActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Form} from '~/components/form/Form';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';

export const DeviceRevokeModal = observer(({sessionIdHashes}: {sessionIdHashes: Array<string>}) => {
	const {t} = useLingui();
	const form = useForm();
	const sessionCount = sessionIdHashes.length;

	const title =
		sessionCount === 0
			? t`Log out all other devices`
			: sessionCount === 1
				? t`Log out 1 device`
				: t`Log out ${sessionCount} devices`;

	const onSubmit = async () => {
		await AuthSessionActionCreators.logout(sessionIdHashes);
		ModalActionCreators.pop();
		ToastActionCreators.createToast({type: 'success', children: t`Device revoked`});
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'form',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit}>
				<Modal.Header title={title} />
				<Modal.Content>
					This will log out the selected {sessionCount === 1 ? t`device` : t`devices`} from your account. You will need
					to log in again on those {sessionCount === 1 ? t`device` : t`devices`}.
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
