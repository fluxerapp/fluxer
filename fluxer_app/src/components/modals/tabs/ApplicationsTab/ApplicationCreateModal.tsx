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
import React from 'react';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {Input} from '~/components/form/Input';
import {Form} from '~/components/form/Form';
import * as Modal from '~/components/modals/Modal';
import styles from '~/components/modals/tabs/ApplicationsTab/ApplicationsTab.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import type {DeveloperApplication} from '~/records/DeveloperApplicationRecord';
import {useFormSubmit} from '~/hooks/useFormSubmit';

interface ApplicationCreateModalProps {
	onCreated: (application: DeveloperApplication) => void;
}

interface CreateFormValues {
	name: string;
}

export const ApplicationCreateModal: React.FC<ApplicationCreateModalProps> = observer(({onCreated}) => {
	const {t} = useLingui();
	const form = useForm<CreateFormValues>({
		defaultValues: {
			name: '',
		},
	});
	const nameField = form.register('name', {required: true, maxLength: 100});
	const nameInputRef = React.useRef<HTMLInputElement | null>(null);
	const handleCancel = React.useCallback(() => {
		form.reset();
		form.clearErrors();
		ModalActionCreators.pop();
	}, [form]);

	const onSubmit = React.useCallback(
		async (data: CreateFormValues) => {
			const response = await HttpClient.post<DeveloperApplication>(Endpoints.OAUTH_APPLICATIONS, {
				name: data.name.trim(),
				redirect_uris: [],
			});
			onCreated(response.body);
			form.reset();
			ModalActionCreators.pop();
		},
		[form, onCreated],
	);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	return (
		<Modal.Root size="small" centered initialFocusRef={nameInputRef}>
			<Form form={form} onSubmit={handleSubmit}>
				<Modal.Header title={t`Create Application`} />
				<Modal.Content className={styles.createForm}>
					<Input
						type="text"
						label={t`Application Name`}
						{...nameField}
						ref={(el) => {
							nameField.ref(el);
							nameInputRef.current = el;
						}}
						placeholder={t`My Application`}
						maxLength={100}
						required
						disabled={isSubmitting}
						autoFocus
						error={form.formState.errors.name?.message}
					/>
				</Modal.Content>
				<Modal.Footer>
					<Button type="button" variant="secondary" onClick={handleCancel} disabled={isSubmitting}>
						{t`Cancel`}
					</Button>
					<Button type="submit" variant="primary" submitting={isSubmitting}>
						{t`Create`}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
