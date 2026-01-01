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
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Form} from '~/components/form/Form';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';

export const GuildDeleteModal = observer(({guildId}: {guildId: string}) => {
	const {t} = useLingui();
	const form = useForm();

	const onSubmit = async () => {
		await GuildActionCreators.remove(guildId);
		ModalActionCreators.pop();
		ToastActionCreators.createToast({type: 'success', children: t`Community deleted`});
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'form',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Delete community form`}>
				<Modal.Header title={t`Delete Community`} />
				<Modal.Content>
					<Trans>
						Are you sure you want to delete this community? This action cannot be undone. All channels, messages, and
						settings will be permanently deleted.
					</Trans>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>I changed my mind</Trans>
					</Button>
					<Button type="submit" submitting={form.formState.isSubmitting} variant="danger-primary">
						<Trans>Delete Community</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
