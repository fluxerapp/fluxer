// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import styles from '@app/features/channel/components/modals/CreateThreadModal.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import Threads from '@app/features/channel/state/Threads';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Channels from '@app/features/channel/state/Channels';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useForm} from 'react-hook-form';

const CREATE_THREAD_DESCRIPTOR = msg({
	message: 'Create Thread',
	comment: 'Title of the create thread modal.',
});
const THREAD_NAME_DESCRIPTOR = msg({
	message: 'Thread name',
	comment: 'Label for the thread name input in the create thread modal.',
});
const THREAD_NAME_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Enter a thread name',
	comment: 'Placeholder for the thread name input in the create thread modal.',
});
const EXPIRY_DESCRIPTOR = msg({
	message: 'Auto-close after',
	comment: 'Label for the thread expiry selector in the create thread modal.',
});
const SOURCE_MESSAGE_DESCRIPTOR = msg({
	message: 'Starting from message',
	comment: 'Label shown in the create thread modal when the thread is started from a specific message.',
});
const THREAD_CREATED_DESCRIPTOR = msg({
	message: 'Thread created',
	comment: 'Toast shown after a thread is successfully created.',
});

const EXPIRY_OPTIONS = [
	{value: 3600000, label: '1 hour'},
	{value: 86400000, label: '24 hours'},
	{value: 259200000, label: '3 days'},
	{value: 604800000, label: '7 days'},
	{value: 2592000000, label: '30 days'},
] as const;

interface FormInputs {
	name: string;
	expires_in_ms: number;
}

interface CreateThreadModalProps {
	channelId: string;
	sourceMessageId?: string;
	sourceMessagePreview?: string;
}

export const CreateThreadModal = observer(({channelId, sourceMessageId, sourceMessagePreview}: CreateThreadModalProps) => {
	const {i18n} = useLingui();
	const form = useForm<FormInputs>({
		defaultValues: {
			name: '',
			expires_in_ms: 604800000,
		},
	});

	const onSubmit = async (data: FormInputs) => {
		const thread = await ThreadCommands.create(channelId, {
			name: data.name,
			expires_in_ms: data.expires_in_ms,
			source_message_id: sourceMessageId,
		});
		Threads.handleThreadCreate(thread);
		Threads.handleThreadMemberAdd({threadId: thread.id});
		ToastCommands.createToast({type: 'success', children: i18n._(THREAD_CREATED_DESCRIPTOR)});
		ModalCommands.pop();
		const channel = Channels.getChannel(channelId);
		if (channel?.guildId) {
			NavigationCommands.selectThread(channel.guildId, channelId, thread.id);
		}
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	return (
		<Modal.Root size="small" centered data-flx="channel.create-thread-modal.modal-root">
			<Form form={form} onSubmit={handleSubmit} data-flx="channel.create-thread-modal.form.submit">
				<Modal.Header title={i18n._(CREATE_THREAD_DESCRIPTOR)} data-flx="channel.create-thread-modal.modal-header" />
				<Modal.Content contentClassName={styles.content} data-flx="channel.create-thread-modal.modal-content">
					{sourceMessagePreview && (
						<div className={styles.field} data-flx="channel.create-thread-modal.source-message-field">
							<div className={styles.label} data-flx="channel.create-thread-modal.source-message-label">
								{i18n._(SOURCE_MESSAGE_DESCRIPTOR)}
							</div>
							<div className={styles.sourceMessage} data-flx="channel.create-thread-modal.source-message-preview">
								{sourceMessagePreview}
							</div>
						</div>
					)}
					<Input
						data-flx="channel.create-thread-modal.name-input"
						{...form.register('name')}
						autoComplete="off"
						autoFocus={true}
						error={form.formState.errors.name?.message}
						label={i18n._(THREAD_NAME_DESCRIPTOR)}
						maxLength={100}
						minLength={1}
						placeholder={i18n._(THREAD_NAME_PLACEHOLDER_DESCRIPTOR)}
						required={true}
					/>
					<div className={styles.field} data-flx="channel.create-thread-modal.expiry-field">
						<div className={styles.label} data-flx="channel.create-thread-modal.expiry-label">
							{i18n._(EXPIRY_DESCRIPTOR)}
						</div>
						<select
							className={styles.select}
							data-flx="channel.create-thread-modal.expiry-select"
							{...form.register('expires_in_ms', {valueAsNumber: true})}
						>
							{EXPIRY_OPTIONS.map((opt) => (
								<option key={opt.value} value={opt.value}>
									{opt.label}
								</option>
							))}
						</select>
					</div>
				</Modal.Content>
				<Modal.Footer data-flx="channel.create-thread-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="channel.create-thread-modal.button.cancel">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						submitting={form.formState.isSubmitting}
						data-flx="channel.create-thread-modal.button.submit"
					>
						{i18n._(CREATE_THREAD_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
