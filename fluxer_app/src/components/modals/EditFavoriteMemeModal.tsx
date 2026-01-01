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
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {Form} from '~/components/form/Form';
import styles from '~/components/modals/EditFavoriteMemeModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {MemeFormFields} from '~/components/modals/meme-form/MemeFormFields';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import type {FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';

interface EditFavoriteMemeModalProps {
	meme: FavoriteMemeRecord;
}

interface FormInputs {
	name: string;
	altText?: string;
	tags: Array<string>;
}

export const EditFavoriteMemeModal = observer(function EditFavoriteMemeModal({meme}: EditFavoriteMemeModalProps) {
	const {t, i18n} = useLingui();
	const form = useForm<FormInputs>({
		defaultValues: {
			name: meme.name,
			altText: meme.altText || '',
			tags: meme.tags,
		},
	});

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			await FavoriteMemeActionCreators.updateFavoriteMeme(i18n, {
				memeId: meme.id,
				name: data.name !== meme.name ? data.name.trim() : undefined,
				altText: data.altText !== (meme.altText || '') ? data.altText?.trim() || null : undefined,
				tags: JSON.stringify(data.tags) !== JSON.stringify(meme.tags) ? data.tags : undefined,
			});
			ModalActionCreators.pop();
		},
		[meme],
	);

	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Edit Saved Media`} />
			<Modal.Content>
				<Form form={form} onSubmit={handleSave} aria-label={t`Edit saved media form`}>
					<div className={styles.formContainer}>
						<MemeFormFields form={form} />
					</div>
				</Form>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()}>
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleSave} disabled={!form.watch('name')?.trim() || form.formState.isSubmitting}>
					<Trans>Save</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
