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

import {Trans} from '@lingui/react/macro';
import React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import * as WebhookActionCreators from '~/actions/WebhookActionCreators';
import type {WebhookRecord} from '~/records/WebhookRecord';

interface WebhookUpdate {
	id: string;
	name?: string;
	avatar?: string | null;
	channelId?: string;
}

interface UseWebhookUpdatesArgs {
	tabId: string;
	canManage: boolean;
	originals: ReadonlyArray<WebhookRecord> | undefined;
}

export function useWebhookUpdates({tabId, canManage, originals}: UseWebhookUpdatesArgs) {
	const [updates, setUpdates] = React.useState<Map<string, WebhookUpdate>>(new Map());
	const [isSaving, setIsSaving] = React.useState(false);
	const [formVersion, setFormVersion] = React.useState(0);
	const originalsRef = React.useRef(originals);
	originalsRef.current = originals;

	const hasUnsavedChanges = updates.size > 0;

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(tabId, hasUnsavedChanges);
	}, [tabId, hasUnsavedChanges]);

	const reset = React.useCallback(() => {
		setUpdates(new Map());
		setFormVersion((v) => v + 1);
	}, []);

	const save = React.useCallback(async () => {
		if (!canManage) return;

		try {
			setIsSaving(true);

			const moves = Array.from(updates.values())
				.filter((u) => u.channelId !== undefined)
				.map((u) => ({webhookId: u.id, newChannelId: u.channelId!}));

			for (const m of moves) {
				await WebhookActionCreators.moveWebhook(m.webhookId, m.newChannelId);
			}

			const basics = Array.from(updates.values())
				.filter((u) => u.name !== undefined || u.avatar !== undefined)
				.map((u) => ({webhookId: u.id, name: u.name, avatar: u.avatar}));

			if (basics.length > 0) {
				await WebhookActionCreators.updateWebhooks(basics);
			}

			setUpdates(new Map());
			setFormVersion((v) => v + 1);
			ToastActionCreators.createToast({type: 'success', children: <Trans>Webhooks updated</Trans>});
		} catch (error) {
			console.error('Failed to update webhooks:', error);
			ToastActionCreators.createToast({type: 'error', children: <Trans>Failed to update webhooks</Trans>});
		} finally {
			setIsSaving(false);
		}
	}, [canManage, updates]);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(tabId, {
			onReset: reset,
			onSave: save,
			isSubmitting: isSaving,
		});
	}, [tabId, reset, save, isSaving]);

	const handleUpdate = React.useCallback((webhookId: string, patch: Partial<WebhookUpdate>) => {
		setUpdates((prev) => {
			const next = new Map(prev);
			const existing = next.get(webhookId) || {id: webhookId};
			const merged: WebhookUpdate = {...existing, ...patch};

			const original = originalsRef.current?.find((w) => w.id === webhookId);
			if (!original) {
				next.set(webhookId, merged);
				return next;
			}

			const changed =
				(merged.name !== undefined && merged.name !== original.name) ||
				(merged.avatar !== undefined && merged.avatar !== original.avatar) ||
				(merged.channelId !== undefined && merged.channelId !== original.channelId);

			if (changed) next.set(webhookId, merged);
			else next.delete(webhookId);

			return next;
		});
	}, []);

	return {updates, hasUnsavedChanges, handleUpdate, reset, save, setUpdates, formVersion};
}
