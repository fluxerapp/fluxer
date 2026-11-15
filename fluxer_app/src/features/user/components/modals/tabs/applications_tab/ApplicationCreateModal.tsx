// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import type {DeveloperApplication} from '@app/features/devtools/models/DeveloperApplication';
import {CANCEL_DESCRIPTOR, CREATE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {http} from '@app/features/platform/transport/RestTransport';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {
	APPLICATION_NAME_DESCRIPTOR,
	CREATE_APPLICATION_DESCRIPTOR,
	MY_APPLICATION_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';
import {useForm} from 'react-hook-form';

interface ApplicationCreateModalProps {
	onCreated: (application: DeveloperApplication) => void;
}

interface CreateFormValues {
	name: string;
}

export const ApplicationCreateModal: React.FC<ApplicationCreateModalProps> = observer(({onCreated}) => {
	const {i18n} = useLingui();
	const form = useForm<CreateFormValues>({
		defaultValues: {
			name: '',
		},
	});
	const nameField = form.register('name', {required: true, maxLength: 100});
	const nameInputRef = useRef<HTMLInputElement | null>(null);
	const handleCancel = useCallback(() => {
		form.reset();
		form.clearErrors();
		ModalCommands.pop();
	}, [form]);
	const onSubmit = useCallback(
		async (data: CreateFormValues) => {
			const response = await http.post<DeveloperApplication>(Endpoints.OAUTH_APPLICATIONS, {
				body: {
					name: data.name.trim(),
					redirect_uris: [],
				},
			});
			onCreated(response.body);
			form.reset();
			ModalCommands.pop();
		},
		[form, onCreated],
	);
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	return (
		<Modal.Root
			size="small"
			centered
			initialFocusRef={nameInputRef}
			data-flx="user.applications-tab.application-create-modal.modal-root"
		>
			<Form form={form} onSubmit={handleSubmit} data-flx="user.applications-tab.application-create-modal.form.submit">
				<Modal.Header
					title={i18n._(CREATE_APPLICATION_DESCRIPTOR)}
					data-flx="user.applications-tab.application-create-modal.modal-header"
				/>
				<Modal.Content data-flx="user.applications-tab.application-create-modal.create-form">
					<Modal.ContentLayout data-flx="user.applications-tab.application-create-modal.modal-content-layout">
						<Input
							type="text"
							label={i18n._(APPLICATION_NAME_DESCRIPTOR)}
							data-flx="user.applications-tab.application-create-modal.input.text"
							{...nameField}
							ref={(el) => {
								nameField.ref(el);
								nameInputRef.current = el;
							}}
							placeholder={i18n._(MY_APPLICATION_DESCRIPTOR)}
							maxLength={100}
							required
							disabled={isSubmitting}
							autoFocus
							error={form.formState.errors.name?.message}
						/>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="user.applications-tab.application-create-modal.modal-footer">
					<Button
						type="button"
						variant="secondary"
						onClick={handleCancel}
						disabled={isSubmitting}
						data-flx="user.applications-tab.application-create-modal.button.cancel"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						variant="primary"
						submitting={isSubmitting}
						data-flx="user.applications-tab.application-create-modal.button.submit"
					>
						{i18n._(CREATE_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
