// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import * as GuildStickerCommands from '@app/features/expressions/commands/GuildStickerCommands';
import styles from '@app/features/expressions/components/modals/EditGuildStickerModal.module.css';
import {StickerFormFields} from '@app/features/expressions/components/modals/sticker_form/StickerFormFields';
import {StickerPreview} from '@app/features/expressions/components/modals/sticker_form/StickerPreview';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as FormUtils from '@app/lib/forms';
import type {GuildStickerWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';
import {useForm} from 'react-hook-form';

const FAILED_TO_UPDATE_STICKER_DESCRIPTOR = msg({
	message: 'Failed to update sticker',
	comment: 'Error toast shown when updating a sticker fails.',
});
const EDIT_STICKER_DESCRIPTOR = msg({
	message: 'Edit sticker',
	comment: 'Action that opens the edit-sticker modal.',
});
const logger = new Logger('EditGuildStickerModal');

interface EditGuildStickerModalProps {
	guildId: string;
	sticker: GuildStickerWithUser;
	onUpdate: () => void;
}

interface FormInputs {
	name: string;
	description: string;
	tags: Array<string>;
}

export const EditGuildStickerModal = observer(function EditGuildStickerModal({
	guildId,
	sticker,
	onUpdate,
}: EditGuildStickerModalProps) {
	const {i18n} = useLingui();
	const {shouldAnimate} = useStickerAnimation({isAnimated: sticker.animated});
	const form = useForm<FormInputs>({
		defaultValues: {
			name: sticker.name,
			description: sticker.description,
			tags: [...sticker.tags],
		},
	});
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			try {
				await GuildStickerCommands.update(guildId, sticker.id, {
					name: data.name.trim(),
					description: data.description.trim(),
					tags: data.tags.length > 0 ? data.tags : [],
				});
				onUpdate();
				ModalCommands.pop();
			} catch (error: unknown) {
				logger.error('Failed to update sticker:', error);
				if (error instanceof HttpError) {
					FormUtils.handleError(i18n, form, error, 'name');
				} else {
					form.setError('name', {message: i18n._(FAILED_TO_UPDATE_STICKER_DESCRIPTOR)});
				}
			}
		},
		[guildId, sticker.id, onUpdate, form, i18n],
	);
	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	const stickerUrl = AvatarUtils.getStickerURL({
		id: sticker.id,
		animated: shouldAnimate,
		isAnimatable: sticker.animated,
		size: 320,
	});
	return (
		<Modal.Root size="small" centered data-flx="expressions.edit-guild-sticker-modal.modal-root">
			<Modal.Header
				title={i18n._(EDIT_STICKER_DESCRIPTOR)}
				data-flx="expressions.edit-guild-sticker-modal.modal-header"
			/>
			<Modal.Content data-flx="expressions.edit-guild-sticker-modal.modal-content">
				<Form form={form} onSubmit={handleSave} data-flx="expressions.edit-guild-sticker-modal.form.save">
					<div className={styles.content} data-flx="expressions.edit-guild-sticker-modal.content">
						<StickerPreview
							imageUrl={stickerUrl}
							altText={sticker.name}
							data-flx="expressions.edit-guild-sticker-modal.sticker-preview"
						/>
						<StickerFormFields form={form} data-flx="expressions.edit-guild-sticker-modal.sticker-form-fields" />
					</div>
				</Form>
			</Modal.Content>
			<Modal.Footer data-flx="expressions.edit-guild-sticker-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					data-flx="expressions.edit-guild-sticker-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					onClick={handleSave}
					disabled={!form.watch('name')?.trim() || form.formState.isSubmitting}
					data-flx="expressions.edit-guild-sticker-modal.button.save"
				>
					<Trans>Save</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
