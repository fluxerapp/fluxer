// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {GuildEventCalendarExportButton} from '@app/features/guild/components/GuildEventCalendarExportButton';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildEventsTab.module.css';
import {
	type GuildEventDateSnapshot,
	guildEventDateToIso,
	snapshotGuildEventDate,
} from '@app/features/guild/utils/GuildEventDateUtils';
import {GuildEventImageReader} from '@app/features/guild/utils/GuildEventImageReader';
import Permission from '@app/features/permissions/state/Permission';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildEvent} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {CalendarIcon, ImageIcon, MapPinIcon, PencilSimpleIcon, PlusIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

interface Draft {
	name: string;
	description: string;
	location: string;
	startsAt: string;
	endsAt: string;
	image: string | null | undefined;
	originalDates?: {
		startsAt: GuildEventDateSnapshot;
		endsAt: GuildEventDateSnapshot | null;
	};
}

const emptyDraft = (): Draft => ({
	name: '',
	description: '',
	location: '',
	startsAt: '',
	endsAt: '',
	image: undefined,
});

const GuildEventsTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const [events, setEvents] = useState<Array<GuildEvent>>([]);
	const [draft, setDraft] = useState<Draft>(emptyDraft);
	const [editingId, setEditingId] = useState<string | null>(null);
	const [busy, setBusy] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const [readingImage, setReadingImage] = useState(false);
	const [imageReader] = useState(
		() =>
			new GuildEventImageReader({
				onLoad: (image) => setDraft((d) => ({...d, image})),
				onError: (cause) => setError(cause.message),
				onPending: setReadingImage,
			}),
	);
	useEffect(() => () => imageReader.cancel(false), [imageReader]);
	const permissions = Permission.getGuildPermissions(guildId) ?? 0n;
	const canCreate =
		(permissions & Permissions.CREATE_EVENTS) === Permissions.CREATE_EVENTS ||
		(permissions & Permissions.MANAGE_EVENTS) === Permissions.MANAGE_EVENTS;
	const canManage = (permissions & Permissions.MANAGE_EVENTS) === Permissions.MANAGE_EVENTS;
	const currentUserId = Users.getCurrentUser()?.id ?? null;

	const refresh = useCallback(async () => {
		try {
			setEvents(await GuildCommands.fetchGuildEvents(guildId));
			setError(null);
		} catch (cause) {
			setError(cause instanceof Error ? cause.message : 'Unable to load community events');
		}
	}, [guildId]);

	useEffect(() => {
		void refresh();
	}, [refresh]);

	const sortedEvents = useMemo(
		() => [...events].sort((a, b) => Date.parse(a.starts_at) - Date.parse(b.starts_at)),
		[events],
	);

	const reset = useCallback(() => {
		imageReader.cancel();
		setEditingId(null);
		setDraft(emptyDraft());
	}, [imageReader]);

	const edit = useCallback(
		(event: GuildEvent) => {
			imageReader.cancel();
			const startsAt = snapshotGuildEventDate(event.starts_at);
			const endsAt = event.ends_at ? snapshotGuildEventDate(event.ends_at) : null;
			setEditingId(event.id);
			setDraft({
				name: event.name,
				description: event.description ?? '',
				location: event.location ?? '',
				startsAt: startsAt.input,
				endsAt: endsAt?.input ?? '',
				image: undefined,
				originalDates: {startsAt, endsAt},
			});
		},
		[imageReader],
	);

	const submit = useCallback(
		async (event: React.FormEvent) => {
			event.preventDefault();
			if (busy || readingImage || !draft.startsAt || !draft.name.trim()) return;
			setBusy(true);
			setError(null);
			try {
				const payload = {
					name: draft.name.trim(),
					description: draft.description.trim() || null,
					location: draft.location.trim() || null,
					starts_at: guildEventDateToIso(draft.startsAt, draft.originalDates?.startsAt),
					ends_at: draft.endsAt ? guildEventDateToIso(draft.endsAt, draft.originalDates?.endsAt) : null,
					...(draft.image !== undefined ? {image: draft.image} : {}),
				};
				if (editingId) {
					await GuildCommands.updateGuildEvent(guildId, editingId, payload);
				} else {
					await GuildCommands.createGuildEvent(guildId, payload);
				}
				reset();
				await refresh();
			} catch (cause) {
				setError(cause instanceof Error ? cause.message : 'Unable to save community event');
			} finally {
				setBusy(false);
			}
		},
		[busy, draft, editingId, guildId, readingImage, refresh, reset],
	);

	const remove = useCallback(
		async (eventId: string) => {
			setBusy(true);
			setError(null);
			try {
				await GuildCommands.deleteGuildEvent(guildId, eventId);
				if (editingId === eventId) reset();
				await refresh();
			} catch (cause) {
				setError(cause instanceof Error ? cause.message : 'Unable to delete community event');
			} finally {
				setBusy(false);
			}
		},
		[editingId, guildId, refresh, reset],
	);

	return (
		<div className={styles.root} data-flx="guild.events-tab">
			<header className={styles.header}>
				<div>
					<h2 className={styles.title}>Community events</h2>
					<p className={styles.subtitle}>
						Publish upcoming events to the community calendar. Event artwork is safety-scanned before it is stored.
					</p>
				</div>
				<CalendarIcon size={28} weight="duotone" aria-hidden />
			</header>

			{error && <div className={styles.error}>{error}</div>}

			<GuildEventCalendarExportButton guildId={guildId} events={sortedEvents} disabled={busy} onError={setError} />

			{canCreate && (
				<form className={styles.editor} onSubmit={submit}>
					<div className={styles.editorHeading}>
						<strong>{editingId ? 'Edit event' : 'Create event'}</strong>
						{editingId && (
							<button type="button" className={styles.linkButton} onClick={reset}>
								Cancel edit
							</button>
						)}
					</div>
					<label>
						<span>Name</span>
						<input
							value={draft.name}
							maxLength={100}
							required
							onChange={(e) => setDraft((d) => ({...d, name: e.target.value}))}
						/>
					</label>
					<div className={styles.twoColumns}>
						<label>
							<span>Starts</span>
							<input
								type="datetime-local"
								value={draft.startsAt}
								required
								onChange={(e) => setDraft((d) => ({...d, startsAt: e.target.value}))}
							/>
						</label>
						<label>
							<span>Ends</span>
							<input
								type="datetime-local"
								value={draft.endsAt}
								onChange={(e) => setDraft((d) => ({...d, endsAt: e.target.value}))}
							/>
						</label>
					</div>
					<label>
						<span>Location</span>
						<input
							value={draft.location}
							maxLength={200}
							onChange={(e) => setDraft((d) => ({...d, location: e.target.value}))}
						/>
					</label>
					<label>
						<span>Description</span>
						<textarea
							value={draft.description}
							maxLength={2000}
							rows={4}
							onChange={(e) => setDraft((d) => ({...d, description: e.target.value}))}
						/>
					</label>
					<label className={styles.imagePicker} aria-busy={readingImage}>
						<ImageIcon size={18} aria-hidden />
						<span>{editingId ? 'Replace event image' : 'Event image'}</span>
						<input
							type="file"
							accept="image/*"
							disabled={busy}
							onChange={(e) => {
								const file = e.currentTarget.files?.[0];
								e.currentTarget.value = '';
								if (file) {
									setError(null);
									imageReader.read(file);
								} else {
									imageReader.cancel();
								}
							}}
						/>
					</label>
					{editingId && (
						<label className={styles.removeImage}>
							<input
								type="checkbox"
								checked={draft.image === null}
								disabled={busy}
								onChange={(e) => {
									imageReader.cancel();
									setDraft((d) => ({...d, image: e.target.checked ? null : undefined}));
								}}
							/>
							<span>Remove current image</span>
						</label>
					)}
					<button className={styles.primaryButton} type="submit" disabled={busy || readingImage}>
						<PlusIcon size={16} aria-hidden />
						{editingId ? 'Save changes' : 'Create event'}
					</button>
				</form>
			)}

			<section className={styles.calendar}>
				{sortedEvents.length === 0 ? (
					<div className={styles.empty}>No community events are scheduled.</div>
				) : (
					sortedEvents.map((event) => {
						const canEdit = canManage || event.creator_id === currentUserId;
						return (
							<article className={styles.card} key={event.id}>
								{event.image_url && <img className={styles.image} src={event.image_url} alt="" />}
								<div className={styles.cardBody}>
									<div className={styles.when}>
										{new Date(event.starts_at).toLocaleString()}
										{event.ends_at ? ` – ${new Date(event.ends_at).toLocaleString()}` : ''}
									</div>
									<h3>{event.name}</h3>
									{event.location && (
										<div className={styles.location}>
											<MapPinIcon size={15} aria-hidden /> {event.location}
										</div>
									)}
									{event.description && <p>{event.description}</p>}
									<div className={styles.actions}>
										<GuildEventCalendarExportButton
											guildId={guildId}
											events={[event]}
											eventId={event.id}
											disabled={busy}
											onError={setError}
										/>
									</div>
									{canEdit && (
										<div className={styles.actions}>
											<button type="button" onClick={() => edit(event)} disabled={busy}>
												<PencilSimpleIcon size={15} aria-hidden />
												<span>Edit</span>
											</button>
											<button type="button" onClick={() => void remove(event.id)} disabled={busy}>
												<TrashIcon size={15} aria-hidden />
												<span>Delete</span>
											</button>
										</div>
									)}
								</div>
							</article>
						);
					})
				)}
			</section>
		</div>
	);
});

export default GuildEventsTab;
