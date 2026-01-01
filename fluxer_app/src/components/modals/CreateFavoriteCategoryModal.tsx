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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import styles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import FavoritesStore from '~/stores/FavoritesStore';

interface FormInputs {
	name: string;
}

export const CreateFavoriteCategoryModal = observer(() => {
	const {t} = useLingui();
	const form = useForm<FormInputs>({
		defaultValues: {
			name: '',
		},
	});

	const onSubmit = async (data: FormInputs) => {
		FavoritesStore.createCategory(data.name);
		ModalActionCreators.pop();
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Create favorite category form`}>
				<Modal.Header title={t`Create Category`} />
				<Modal.Content className={styles.content}>
					<Input
						{...form.register('name')}
						autoFocus={true}
						autoComplete="off"
						error={form.formState.errors.name?.message}
						label={t`Category Name`}
						maxLength={100}
						minLength={1}
						placeholder={t`New Category`}
						required={true}
					/>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						{t`Cancel`}
					</Button>
					<Button type="submit" submitting={form.formState.isSubmitting}>
						{t`Create Category`}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
