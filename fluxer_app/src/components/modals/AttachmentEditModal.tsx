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
import {MessageAttachmentFlags} from '~/Constants';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import {Switch} from '~/components/form/Switch';
import styles from '~/components/modals/AttachmentEditModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {type CloudAttachment, CloudUpload} from '~/lib/CloudUpload';

interface FormInputs {
	filename: string;
	spoiler: boolean;
}

export const AttachmentEditModal = observer(
	({channelId, attachment}: {channelId: string; attachment: CloudAttachment}) => {
		const {t} = useLingui();
		const defaultSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;

		const form = useForm<FormInputs>({
			defaultValues: {
				filename: attachment.filename,
				spoiler: defaultSpoiler,
			},
		});

		const onSubmit = async (data: FormInputs) => {
			const nextFlags = data.spoiler
				? attachment.flags | MessageAttachmentFlags.IS_SPOILER
				: attachment.flags & ~MessageAttachmentFlags.IS_SPOILER;

			CloudUpload.updateAttachment(channelId, attachment.id, {
				filename: data.filename,
				flags: nextFlags,
				spoiler: data.spoiler,
			});

			ModalActionCreators.pop();
		};

		return (
			<Modal.Root size="small" centered>
				<Form form={form} onSubmit={onSubmit} aria-label={t`Edit attachment form`}>
					<Modal.Header title={attachment.filename} />
					<Modal.Content className={styles.content}>
						<Input
							{...form.register('filename')}
							autoFocus={true}
							label={t`Filename`}
							minLength={1}
							maxLength={512}
							required={true}
							type="text"
							spellCheck={false}
						/>
						<Switch
							label={t`Mark as spoiler`}
							value={form.watch('spoiler')}
							onChange={(value) => form.setValue('spoiler', value)}
						/>
					</Modal.Content>
					<Modal.Footer>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button type="submit">
							<Trans>Save</Trans>
						</Button>
					</Modal.Footer>
				</Form>
			</Modal.Root>
		);
	},
);
