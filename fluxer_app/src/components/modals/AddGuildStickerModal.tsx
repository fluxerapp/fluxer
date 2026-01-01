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
import React from 'react';
import {useForm} from 'react-hook-form';
import * as GuildStickerActionCreators from '~/actions/GuildStickerActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {STICKER_MAX_SIZE} from '~/Constants';
import {Form} from '~/components/form/Form';
import styles from '~/components/modals/AddGuildStickerModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {StickerFormFields} from '~/components/modals/sticker-form/StickerFormFields';
import {StickerPreview} from '~/components/modals/sticker-form/StickerPreview';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import * as ImageCropUtils from '~/utils/ImageCropUtils';

interface AddGuildStickerModalProps {
	guildId: string;
	file: File;
	onSuccess: () => void;
}

interface FormInputs {
	name: string;
	description: string;
	tags: Array<string>;
}

export const AddGuildStickerModal = observer(function AddGuildStickerModal({
	guildId,
	file,
	onSuccess,
}: AddGuildStickerModalProps) {
	const {t} = useLingui();
	const [isProcessing, setIsProcessing] = React.useState(false);
	const [previewUrl, setPreviewUrl] = React.useState<string | null>(null);

	const form = useForm<FormInputs>({
		defaultValues: {
			name: GuildStickerActionCreators.sanitizeStickerName(file.name),
			description: '',
			tags: [],
		},
	});

	React.useEffect(() => {
		const url = URL.createObjectURL(file);
		setPreviewUrl(url);
		return () => URL.revokeObjectURL(url);
	}, [file]);

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			setIsProcessing(true);
			try {
				const base64Image = await ImageCropUtils.optimizeStickerImage(file, STICKER_MAX_SIZE, 320);

				await GuildStickerActionCreators.create(guildId, {
					name: data.name.trim(),
					description: data.description.trim(),
					tags: data.tags.length > 0 ? data.tags : [],
					image: base64Image,
				});

				onSuccess();
				ModalActionCreators.pop();
			} catch (error: any) {
				console.error('Failed to create sticker:', error);
				form.setError('name', {
					message: error.message || t`Failed to create sticker`,
				});
				setIsProcessing(false);
			}
		},
		[guildId, file, onSuccess, form],
	);

	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Add Sticker`} />
			<Modal.Content>
				<Form form={form} onSubmit={handleSave} aria-label={t`Add sticker form`}>
					<div className={styles.formContainer}>
						{previewUrl && <StickerPreview imageUrl={previewUrl} altText={form.watch('name') || file.name} />}
						<StickerFormFields form={form} disabled={isProcessing} />
					</div>
				</Form>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()} disabled={isProcessing}>
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleSave} disabled={!form.watch('name')?.trim() || isProcessing} submitting={isProcessing}>
					<Trans>Create</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
