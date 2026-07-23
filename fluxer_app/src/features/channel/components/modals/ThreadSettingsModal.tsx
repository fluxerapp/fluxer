// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import styles from '@app/features/channel/components/modals/ThreadSettingsModal.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import Threads from '@app/features/channel/state/Threads';
import Permission from '@app/features/permissions/state/Permission';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';
import {useForm} from 'react-hook-form';

const OVERVIEW_DESCRIPTOR = msg({message: 'Overview', comment: 'Thread settings overview nav item.'});
const DELETE_THREAD_DESCRIPTOR = msg({message: 'Delete Thread', comment: 'Delete thread nav item in settings.'});
const THREAD_NAME_DESCRIPTOR = msg({message: 'Thread Name', comment: 'Label for thread name input in settings.'});
const SLOWMODE_DESCRIPTOR = msg({message: 'Slowmode', comment: 'Label for slowmode selector in thread settings.'});
const INACTIVITY_DESCRIPTOR = msg({message: 'Hide After Inactivity', comment: 'Label for auto-archive selector in thread settings.'});
const SAVE_CHANGES_DESCRIPTOR = msg({message: 'Save Changes', comment: 'Save button in thread settings modal.'});
const CHANGES_SAVED_DESCRIPTOR = msg({message: 'Changes saved', comment: 'Toast shown after saving thread settings.'});
const DELETE_CONFIRM_DESCRIPTOR = msg({message: 'Delete thread?', comment: 'Confirm delete thread title in settings.'});
const DELETE_BODY_DESCRIPTOR = msg({message: 'This will permanently delete the thread and all its messages. This cannot be undone.', comment: 'Confirm delete thread body in settings.'});
const CONFIRM_DESCRIPTOR = msg({message: 'Confirm', comment: 'Confirm button in thread settings delete confirm.'});

const SLOWMODE_OPTIONS = [
	{value: 0, label: 'Off'},
	{value: 5, label: '5 seconds'},
	{value: 10, label: '10 seconds'},
	{value: 15, label: '15 seconds'},
	{value: 30, label: '30 seconds'},
	{value: 60, label: '1 minute'},
	{value: 120, label: '2 minutes'},
	{value: 300, label: '5 minutes'},
	{value: 600, label: '10 minutes'},
	{value: 900, label: '15 minutes'},
	{value: 1800, label: '30 minutes'},
	{value: 3600, label: '1 hour'},
] as const;

const INACTIVITY_OPTIONS = [
	{value: 3600000, label: '1 hour'},
	{value: 86400000, label: '24 hours'},
	{value: 259200000, label: '3 days'},
	{value: 604800000, label: '1 week'},
] as const;

type NavPage = 'overview' | 'delete';

interface FormInputs {
	name: string;
	rate_limit_per_user: number;
	expires_in_ms: number;
}

interface ThreadSettingsModalProps {
	threadId: string;
	parentChannelId: string;
}

export const ThreadSettingsModal = observer(({threadId, parentChannelId}: ThreadSettingsModalProps) => {
	const {i18n} = useLingui();
	const thread = Threads.getThread(threadId);
	const canManage = thread ? Permission.can(Permissions.MANAGE_THREADS, thread.toChannel()) : false;
	const [page, setPage] = useState<NavPage>('overview');
	const [confirmDelete, setConfirmDelete] = useState(false);

	const form = useForm<FormInputs>({
		defaultValues: {
			name: thread?.name ?? '',
			rate_limit_per_user: thread?.toChannel().rateLimitPerUser ?? 0,
			expires_in_ms: 604800000,
		},
	});

	const threadGroupLabel = thread?.name?.toUpperCase() ?? 'THREAD';

	const onSubmit = async (data: FormInputs) => {
		const durationMs = data.expires_in_ms;
		const autoArchive = durationMs <= 3_600_000 ? 60 : durationMs <= 86_400_000 ? 1440 : durationMs <= 259_200_000 ? 4320 : 10080;
		await ThreadCommands.update(parentChannelId, threadId, {
			name: data.name || undefined,
			rate_limit_per_user: data.rate_limit_per_user,
			expires_in_ms: data.expires_in_ms,
			auto_archive_duration: autoArchive,
		});
		ToastCommands.createToast({type: 'success', children: i18n._(CHANGES_SAVED_DESCRIPTOR)});
		ModalCommands.pop();
	};

	const {handleSubmit} = useFormSubmit({form, onSubmit, defaultErrorField: 'name'});

	const handleDelete = async () => {
		if (!confirmDelete) {
			setConfirmDelete(true);
			return;
		}
		await ThreadCommands.remove(parentChannelId, threadId);
		ModalCommands.pop();
	};

	return (
		<Modal.Root size="large" data-flx="channel.thread-settings-modal.root">
			<Modal.ScreenReaderLabel text={threadGroupLabel} />
			<div className={styles.layout} data-flx="channel.thread-settings-modal.layout">
				<nav className={styles.sidebar} data-flx="channel.thread-settings-modal.sidebar">
					<div className={styles.sidebarGroupLabel} data-flx="channel.thread-settings-modal.group-label">
						{threadGroupLabel} GENERAL
					</div>
					<button
						type="button"
						className={clsx(styles.navItem, page === 'overview' ? styles.navItemActive : undefined)}
						onClick={() => setPage('overview')}
						data-flx="channel.thread-settings-modal.nav-overview"
					>
						{i18n._(OVERVIEW_DESCRIPTOR)}
					</button>
					{canManage && (
						<button
							type="button"
							className={clsx(styles.navItem, styles.navItemDanger)}
							onClick={() => setPage('delete')}
							data-flx="channel.thread-settings-modal.nav-delete"
						>
							<TrashIcon size={16} aria-hidden="true" />
							{i18n._(DELETE_THREAD_DESCRIPTOR)}
						</button>
					)}
				</nav>

				<div className={styles.main} data-flx="channel.thread-settings-modal.main">
					<Modal.InsetCloseButton
						onClick={ModalCommands.pop}
						data-flx="channel.thread-settings-modal.close-button"
					/>

					{page === 'overview' && (
						<Form form={form} onSubmit={handleSubmit} data-flx="channel.thread-settings-modal.form">
							<h2 className={styles.pageTitle} data-flx="channel.thread-settings-modal.page-title">
								{i18n._(OVERVIEW_DESCRIPTOR)}
							</h2>
							<div className={styles.fields} data-flx="channel.thread-settings-modal.fields">
								<Input
									{...form.register('name')}
									label={i18n._(THREAD_NAME_DESCRIPTOR)}
									maxLength={36}
									autoComplete="off"
									error={form.formState.errors.name?.message}
									data-flx="channel.thread-settings-modal.name-input"
								/>
								{canManage && (
									<>
										<div className={styles.field} data-flx="channel.thread-settings-modal.slowmode-field">
											<div className={styles.fieldLabel} data-flx="channel.thread-settings-modal.slowmode-label">
												{i18n._(SLOWMODE_DESCRIPTOR)}
											</div>
											<select
												className={styles.select}
												{...form.register('rate_limit_per_user', {valueAsNumber: true})}
												data-flx="channel.thread-settings-modal.slowmode-select"
											>
												{SLOWMODE_OPTIONS.map((opt) => (
													<option key={opt.value} value={opt.value}>
														{opt.label}
													</option>
												))}
											</select>
											<p className={styles.hint} data-flx="channel.thread-settings-modal.slowmode-hint">
												Members will be restricted to sending one message per this interval, unless they have the Bypass Slowmode permission.
											</p>
										</div>
										<div className={styles.field} data-flx="channel.thread-settings-modal.inactivity-field">
											<div className={styles.fieldLabel} data-flx="channel.thread-settings-modal.inactivity-label">
												{i18n._(INACTIVITY_DESCRIPTOR)}
											</div>
											<select
												className={styles.select}
												{...form.register('expires_in_ms', {valueAsNumber: true})}
												data-flx="channel.thread-settings-modal.inactivity-select"
											>
												{INACTIVITY_OPTIONS.map((opt) => (
													<option key={opt.value} value={opt.value}>
														{opt.label}
													</option>
												))}
											</select>
											<p className={styles.hint} data-flx="channel.thread-settings-modal.inactivity-hint">
												Threads will not show in the channel list after being inactive for the specified duration.
											</p>
										</div>
									</>
								)}
							</div>
							<div className={styles.footer} data-flx="channel.thread-settings-modal.footer">
								<Button
									onClick={ModalCommands.pop}
									variant="secondary"
									data-flx="channel.thread-settings-modal.cancel-button"
								>
									{i18n._(CANCEL_DESCRIPTOR)}
								</Button>
								<Button
									type="submit"
									submitting={form.formState.isSubmitting}
									data-flx="channel.thread-settings-modal.save-button"
								>
									{i18n._(SAVE_CHANGES_DESCRIPTOR)}
								</Button>
							</div>
						</Form>
					)}

					{page === 'delete' && canManage && (
						<div className={styles.deletePage} data-flx="channel.thread-settings-modal.delete-page">
							<h2 className={styles.pageTitle} data-flx="channel.thread-settings-modal.delete-title">
								{i18n._(DELETE_THREAD_DESCRIPTOR)}
							</h2>
							{!confirmDelete ? (
								<>
									<p className={styles.deleteBody} data-flx="channel.thread-settings-modal.delete-body">
										{i18n._(DELETE_BODY_DESCRIPTOR)}
									</p>
									<div className={styles.footer} data-flx="channel.thread-settings-modal.delete-footer">
										<Button
											onClick={ModalCommands.pop}
											variant="secondary"
											data-flx="channel.thread-settings-modal.delete-cancel"
										>
											{i18n._(CANCEL_DESCRIPTOR)}
										</Button>
										<Button
											variant="danger"
											onClick={handleDelete}
											data-flx="channel.thread-settings-modal.delete-confirm"
										>
											{i18n._(DELETE_THREAD_DESCRIPTOR)}
										</Button>
									</div>
								</>
							) : (
								<>
									<p className={styles.deleteBody} data-flx="channel.thread-settings-modal.delete-body--2">
										{i18n._(DELETE_CONFIRM_DESCRIPTOR)}
									</p>
									<div className={styles.footer} data-flx="channel.thread-settings-modal.delete-footer--2">
										<Button
											onClick={() => setConfirmDelete(false)}
											variant="secondary"
											data-flx="channel.thread-settings-modal.delete-back"
										>
											{i18n._(CANCEL_DESCRIPTOR)}
										</Button>
										<Button
											variant="danger"
											onClick={handleDelete}
											data-flx="channel.thread-settings-modal.delete-confirm--2"
										>
											{i18n._(CONFIRM_DESCRIPTOR)}
										</Button>
									</div>
								</>
							)}
						</div>
					)}
				</div>
			</div>
		</Modal.Root>
	);
});
