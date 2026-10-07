// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import styles from '@app/features/channel/components/modals/ChannelCreateModal.module.css';
import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import {SlowmodeControl} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SlowmodeControl';
import type {FormInputs as ChannelOverviewFormInputs} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import Channels from '@app/features/channel/state/Channels';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {canPatchThread, isThreadModeratorFor} from '@app/features/threads/utils/ThreadActionRules';
import {getAutoArchiveOptions} from '@app/features/threads/utils/ThreadAutoArchiveOptions';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {DEFAULT_THREAD_AUTO_ARCHIVE_DURATION, THREAD_NAME_MAX_LENGTH} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';
import {Controller, type UseFormReturn, useForm} from 'react-hook-form';

interface ThreadSettingsFormInputs {
	name: string;
	slowmode: number;
	auto_archive_duration: number;
	invitable: boolean;
}

export const ThreadSettingsModal = observer(({threadId}: {threadId: string}) => {
	const {i18n} = useLingui();
	const thread = Channels.getChannel(threadId);
	const form = useForm<ThreadSettingsFormInputs>({
		defaultValues: {
			name: thread?.name ?? '',
			slowmode: thread?.rateLimitPerUser ?? 0,
			auto_archive_duration: thread?.threadMetadata?.auto_archive_duration ?? DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
			invitable: thread?.threadMetadata?.invitable ?? true,
		},
	});
	const onSubmit = async (data: ThreadSettingsFormInputs) => {
		if (!thread) return;
		const dirty = form.formState.dirtyFields;
		const body: ThreadCommands.ThreadPatchBody = {};
		if (dirty.name) body.name = data.name.trim();
		if (dirty.slowmode) body.rate_limit_per_user = data.slowmode;
		if (dirty.auto_archive_duration) body.auto_archive_duration = data.auto_archive_duration;
		if (dirty.invitable) body.invitable = data.invitable;
		if (Object.keys(body).length > 0) {
			await ThreadCommands.updateThread(thread, body);
		}
		ModalCommands.pop();
	};
	const {handleSubmit} = useFormSubmit({form, onSubmit, defaultErrorField: 'name'});
	useEffect(() => {
		if (!thread) ModalCommands.pop();
	}, [thread]);
	if (!thread) return null;
	const moderator = isThreadModeratorFor(thread);
	const canRename = canPatchThread(thread, {name: thread.name});
	return (
		<Modal.Root size="small" centered data-flx="threads.thread-settings-modal.modal-root">
			<Form form={form} onSubmit={handleSubmit} data-flx="threads.thread-settings-modal.form.submit">
				<Modal.Header
					title={i18n._(D.THREAD_SETTINGS_DESCRIPTOR)}
					data-flx="threads.thread-settings-modal.modal-header"
				/>
				<Modal.Content contentClassName={styles.content} data-flx="threads.thread-settings-modal.modal-content">
					<Input
						data-flx="threads.thread-settings-modal.input.name"
						{...form.register('name')}
						autoComplete="off"
						disabled={!canRename}
						error={form.formState.errors.name?.message}
						label={i18n._(D.THREAD_NAME_DESCRIPTOR)}
						maxLength={THREAD_NAME_MAX_LENGTH}
						minLength={1}
						required={true}
					/>
					{canRename && (
						<SettingsControlRow
							label={i18n._(D.HIDE_AFTER_INACTIVITY_DESCRIPTOR)}
							stacked
							dataFlx="threads.thread-settings-modal.auto-archive-section"
							data-flx="threads.thread-settings-modal.auto-archive-section.settings-control-row"
						>
							<Controller
								name="auto_archive_duration"
								control={form.control}
								render={({field}) => (
									<RadioGroup
										aria-label={i18n._(D.HIDE_AFTER_INACTIVITY_DESCRIPTOR)}
										value={field.value}
										onChange={field.onChange}
										options={getAutoArchiveOptions(i18n)}
										data-flx="threads.thread-settings-modal.radio-group.auto-archive"
									/>
								)}
								data-flx="threads.thread-settings-modal.controller.auto-archive"
							/>
						</SettingsControlRow>
					)}
					{moderator && (
						<SlowmodeControl
							form={form as unknown as UseFormReturn<ChannelOverviewFormInputs>}
							data-flx="threads.thread-settings-modal.slowmode-control"
						/>
					)}
					{thread.isPrivateThread() && canRename && (
						<Controller
							name="invitable"
							control={form.control}
							render={({field}) => (
								<Switch
									label={i18n._(D.INVITABLE_DESCRIPTOR)}
									value={field.value}
									onChange={field.onChange}
									data-flx="threads.thread-settings-modal.switch.invitable"
								/>
							)}
							data-flx="threads.thread-settings-modal.controller.invitable"
						/>
					)}
				</Modal.Content>
				<Modal.Footer data-flx="threads.thread-settings-modal.modal-footer">
					<Button
						onClick={ModalCommands.pop}
						variant="secondary"
						data-flx="threads.thread-settings-modal.button.cancel"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						submitting={form.formState.isSubmitting}
						data-flx="threads.thread-settings-modal.button.submit"
					>
						{i18n._(D.SAVE_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
