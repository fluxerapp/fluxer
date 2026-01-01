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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {SoundType} from '~/utils/SoundUtils';

export const getSoundLabels = (i18n: I18n): Record<SoundType, string> => ({
	[SoundType.Message]: i18n._(msg`Message Notifications`),
	[SoundType.Mute]: i18n._(msg`Voice Mute`),
	[SoundType.Unmute]: i18n._(msg`Voice Unmute`),
	[SoundType.Deaf]: i18n._(msg`Voice Deafen`),
	[SoundType.Undeaf]: i18n._(msg`Voice Undeafen`),
	[SoundType.UserJoin]: i18n._(msg`User Joins Channel`),
	[SoundType.UserLeave]: i18n._(msg`User Leaves Channel`),
	[SoundType.UserMove]: i18n._(msg`User Moved Channel`),
	[SoundType.ViewerJoin]: i18n._(msg`Viewer Joins Stream`),
	[SoundType.ViewerLeave]: i18n._(msg`Viewer Leaves Stream`),
	[SoundType.VoiceDisconnect]: i18n._(msg`Voice Disconnected`),
	[SoundType.IncomingRing]: i18n._(msg`Incoming Call`),
	[SoundType.CameraOn]: i18n._(msg`Camera On`),
	[SoundType.CameraOff]: i18n._(msg`Camera Off`),
	[SoundType.ScreenShareStart]: i18n._(msg`Screen Share Start`),
	[SoundType.ScreenShareStop]: i18n._(msg`Screen Share Stop`),
});
