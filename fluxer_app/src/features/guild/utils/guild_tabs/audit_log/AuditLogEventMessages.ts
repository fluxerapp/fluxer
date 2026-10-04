// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const EVENT_CREATE_SUMMARY = msg({
	message: '{actor} created the event {name}',
	comment:
		'Activity log summary for a scheduled community event. {actor} is the member who created it, shown as a user chip, or System. {name} is the recorded event name, shown in bold.',
});
export const EVENT_CREATE_UNNAMED_SUMMARY = msg({
	message: '{actor} created an event',
	comment:
		'Activity log summary for an event whose name was not recorded. {actor} is the member who created it, shown as a user chip, or System.',
});
export const EVENT_UPDATE_SUMMARY = msg({
	message: '{actor} updated the event {name}',
	comment:
		'Activity log summary for an edited community event. {actor} is the member who edited it, shown as a user chip, or System. {name} is the recorded event name, shown in bold.',
});
export const EVENT_UPDATE_UNNAMED_SUMMARY = msg({
	message: '{actor} updated an event',
	comment:
		'Activity log summary for an edited event whose name was not recorded in this entry. {actor} is the member who edited it, shown as a user chip, or System.',
});
export const EVENT_DELETE_SUMMARY = msg({
	message: '{actor} deleted the event {name}',
	comment:
		'Activity log summary for a deleted community event. {actor} is the member who deleted it, shown as a user chip, or System. {name} is the name recorded before deletion, shown in bold.',
});
export const EVENT_DELETE_UNNAMED_SUMMARY = msg({
	message: '{actor} deleted an event',
	comment:
		'Activity log summary for a deleted event whose name was not recorded. {actor} is the member who deleted it, shown as a user chip, or System.',
});
export const EVENT_CREATOR_ROW = msg({
	message: 'The event was created by {user}',
	comment:
		'Activity log detail under a deleted event, shown when another member originally created the event. {user} is the original creator, shown as a user chip.',
});
export const EVENT_NAME_CHANGED_ROW = msg({
	message: 'Changed the event name from {oldValue} to {newValue}',
	comment:
		'Past-tense activity log detail for an event rename. {oldValue} and {newValue} are the old and new event names, shown in bold. This continues the actor in the summary and is not an instruction.',
});
export const EVENT_DESCRIPTION_SET_ROW = msg({
	message: 'Set the event description to {value}',
	comment:
		'Past-tense activity log detail. The actor added this event description. {value} is the recorded description, shown as inline text.',
});
export const EVENT_DESCRIPTION_CHANGED_ROW = msg({
	message: 'Changed the event description from {oldValue} to {newValue}',
	comment:
		'Past-tense activity log detail. {oldValue} and {newValue} are the previous and new event descriptions, shown as inline text.',
});
export const EVENT_DESCRIPTION_REMOVED_ROW = msg({
	message: 'Removed the event description {value}',
	comment: 'Past-tense activity log detail. {value} is the previous event description, shown as inline text.',
});
export const EVENT_LOCATION_SET_ROW = msg({
	message: 'Set the event location to {value}',
	comment: 'Past-tense activity log detail. {value} is the location or URL added to the event, shown as inline text.',
});
export const EVENT_LOCATION_CHANGED_ROW = msg({
	message: 'Changed the event location from {oldValue} to {newValue}',
	comment:
		'Past-tense activity log detail. {oldValue} and {newValue} are the previous and new event locations or URLs, shown as inline text.',
});
export const EVENT_LOCATION_REMOVED_ROW = msg({
	message: 'Removed the event location {value}',
	comment: 'Past-tense activity log detail. {value} is the previous event location or URL, shown as inline text.',
});
export const EVENT_START_SET_ROW = msg({
	message: 'Set the event start time to {value}',
	comment: 'Past-tense activity log detail. {value} is the scheduled start, formatted as a local date and time.',
});
export const EVENT_START_CHANGED_ROW = msg({
	message: 'Changed the event start time from {oldValue} to {newValue}',
	comment:
		'Past-tense activity log detail. {oldValue} and {newValue} are the previous and new scheduled start, formatted as local dates and times.',
});
export const EVENT_END_SET_ROW = msg({
	message: 'Set the event end time to {value}',
	comment: 'Past-tense activity log detail. {value} is the scheduled end, formatted as a local date and time.',
});
export const EVENT_END_CHANGED_ROW = msg({
	message: 'Changed the event end time from {oldValue} to {newValue}',
	comment:
		'Past-tense activity log detail. {oldValue} and {newValue} are the previous and new scheduled end, formatted as local dates and times.',
});
export const EVENT_END_REMOVED_ROW = msg({
	message: 'Removed the event end time {value}',
	comment: 'Past-tense activity log detail. {value} is the previous scheduled end, formatted as a local date and time.',
});
export const EVENT_IMAGE_ADDED_ROW = msg({
	message: 'Added an event image',
	comment: 'Past-tense activity log detail. The actor added an image to an event that previously had none.',
});
export const EVENT_IMAGE_CHANGED_ROW = msg({
	message: 'Changed the event image',
	comment: 'Past-tense activity log detail. The actor replaced the existing event image.',
});
export const EVENT_IMAGE_REMOVED_ROW = msg({
	message: 'Removed the event image',
	comment: 'Past-tense activity log detail. The actor removed the existing event image.',
});
