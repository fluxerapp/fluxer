// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import * as WebhookCommands from '@app/features/webhook/commands/WebhookCommands';
import type {Webhook} from '@app/features/webhook/models/Webhook';
import {Trans, useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('useWebhookUpdates');

interface WebhookUpdate {
	id: string;
	name?: string;
	avatar?: string | null;
	channelId?: string;
}

interface UseWebhookUpdatesArgs {
	tabId: string;
	canManage: boolean;
	originals: ReadonlyArray<Webhook> | undefined;
}

export function useWebhookUpdates({tabId, canManage, originals}: UseWebhookUpdatesArgs) {
	const {i18n} = useLingui();
	const [updates, setUpdates] = useState<Map<string, WebhookUpdate>>(new Map());
	const [isSaving, setIsSaving] = useState(false);
	const [formVersion, setFormVersion] = useState(0);
	const originalsRef = useRef(originals);
	originalsRef.current = originals;
	const hasUnsavedChanges = updates.size > 0;
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(tabId, hasUnsavedChanges);
	}, [tabId, hasUnsavedChanges]);
	const reset = useCallback(() => {
		setUpdates(new Map());
		setFormVersion((v) => v + 1);
	}, []);
	const save = useCallback(async () => {
		if (!canManage) return;
		try {
			setIsSaving(true);
			const moves = Array.from(updates.values())
				.filter((u) => u.channelId !== undefined)
				.map((u) => ({webhookId: u.id, newChannelId: u.channelId!}));
			for (const m of moves) {
				await WebhookCommands.moveWebhook(m.webhookId, m.newChannelId);
			}
			const basics = Array.from(updates.values())
				.filter((u) => u.name !== undefined || u.avatar !== undefined)
				.map((u) => ({webhookId: u.id, name: u.name, avatar: u.avatar}));
			if (basics.length > 0) {
				await WebhookCommands.updateWebhooks(basics);
			}
			setUpdates(new Map());
			setFormVersion((v) => v + 1);
			ToastCommands.createToast({type: 'success', children: <Trans>Webhooks updated</Trans>});
		} catch (error) {
			logger.error('Failed to update webhooks', error);
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: failureMessage(error) ?? <Trans>Failed to update webhooks</Trans>,
				dataFlx: 'webhook.use-webhook-updates.save-error-modal',
			});
		} finally {
			setIsSaving(false);
		}
	}, [canManage, updates, i18n]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(tabId, {
			onReset: reset,
			onSave: save,
			isSubmitting: isSaving,
		});
	}, [tabId, reset, save, isSaving]);
	const handleUpdate = useCallback((webhookId: string, patch: Partial<WebhookUpdate>) => {
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
