import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:fluxer_app/core/permissions/channel_effective_permissions.dart';
import 'package:fluxer_app/core/permissions/permission.dart';
import 'package:fluxer_app/core/theme/fluxer_theme_extension.dart';
import 'package:fluxer_app/features/events/data/events_repository.dart';
import 'package:fluxer_app/features/events/domain/event.dart';
import 'package:fluxer_app/features/events/providers/events_provider.dart';
import 'package:fluxer_app/features/ui/ui.dart';
import 'package:fluxer_app/l10n/generated/fluxer_localizations.dart';
import 'package:fluxer_app/shared/external_links/external_url_launcher.dart';
import 'package:fluxer_app/shared/utils/clipboard_utils.dart';
import 'package:phosphor_flutter/phosphor_flutter.dart';

/// Shows the detailed view modal/sheet for a specific event.
Future<void> showEventDetailSheet(
  BuildContext context, {
  required GuildEvent event,
  required String channelId,
  required String guildId,
}) {
  return FluxerBottomSheet.showScrollable<void>(
    context,
    title: event.name,
    initialChildSize: 0.7,
    minChildSize: 0.4,
    maxChildSize: 0.9,
    builder: (ctx, controller, close) => _EventDetailSheet(
      event: event,
      channelId: channelId,
      guildId: guildId,
      scrollController: controller,
    ),
  );
}

class _EventDetailSheet extends ConsumerWidget {
  const _EventDetailSheet({
    required this.event,
    required this.channelId,
    required this.guildId,
    required this.scrollController,
  });

  final GuildEvent event;
  final String channelId;
  final String guildId;
  final ScrollController scrollController;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final l10n = FluxerLocalizations.of(context);
    final attendeesAsync = ref.watch(
      eventAttendeesProvider((channelId, event.id)),
    );

    final int? permissionBits =
        ref.watch(channelPermissionCacheProvider)[channelId];
    final bool canManageEvent = permissionBits == null ||
        hasPermission(permissionBits, Permission.manageEvents) ||
        hasPermission(permissionBits, Permission.manageChannels);

    final bool isOngoing = event.isOngoing;
    final bool hasEnded = event.hasEnded;

    return ListView(
      controller: scrollController,
      padding: const EdgeInsets.all(16),
      children: [
        // Event Status banner if live
        if (isOngoing) ...[
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
            decoration: BoxDecoration(
              color: context.colors.statusGreen.withValues(alpha: 0.15),
              borderRadius: BorderRadius.circular(8),
              border: Border.all(
                color: context.colors.statusGreen.withValues(alpha: 0.3),
              ),
            ),
            child: Row(
              children: [
                Icon(
                  Icons.radio_button_checked,
                  color: context.colors.statusGreen,
                  size: 18,
                ),
                const SizedBox(width: 8),
                Text(
                  'Event is currently live!',
                  style: context.textStyles.heading4.copyWith(
                    color: context.colors.statusGreen,
                    fontSize: 14,
                  ),
                ),
              ],
            ),
          ),
          const SizedBox(height: 16),
        ],

        // Date & Time block
        _DetailRow(
          icon: PhosphorIconsRegular.clock,
          title: _formatFullDateRange(event),
        ),

        const SizedBox(height: 12),

        // Location block
        if (event.locationText != null || event.locationChannelId != null) ...[
          _DetailRow(
            icon: event.locationChannelId != null
                ? PhosphorIconsRegular.speakerHigh
                : PhosphorIconsRegular.mapPin,
            title: event.locationText ?? 'Voice Channel',
          ),
          const SizedBox(height: 12),
        ],

        // Repeat info
        if (event.repeatType != EventRepeatType.never) ...[
          _DetailRow(
            icon: PhosphorIconsRegular.arrowsRepeat,
            title: 'Repeats ${_repeatText(event.repeatType)}',
          ),
          const SizedBox(height: 12),
        ],

        const Divider(),
        const SizedBox(height: 12),

        // Description
        if (event.description != null && event.description!.isNotEmpty) ...[
          Text(
            'DESCRIPTION',
            style: context.textStyles.labelSmall.copyWith(
              color: context.colors.textTertiary,
              fontWeight: FontWeight.w700,
            ),
          ),
          const SizedBox(height: 6),
          Text(
            event.description!,
            style: context.textStyles.messageBody.copyWith(
              color: context.colors.textPrimary,
            ),
          ),
          const SizedBox(height: 16),
        ],

        // RSVP Action Button
        Row(
          children: [
            Expanded(
              child: FluxerFilledButton(
                onPressed: () {
                  unawaited(
                    ref
                        .read(channelEventsProvider(channelId).notifier)
                        .toggleRsvp(event.id),
                  );
                },
                color: event.isAttending
                    ? context.colors.backgroundTertiary
                    : context.colors.brandExperiment,
                child: Text(
                  event.isAttending ? '✓ Confirmed (Going)' : "I'm Going!",
                  style: TextStyle(
                    color: event.isAttending
                        ? context.colors.textPrimary
                        : Colors.white,
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ),
            ),
            const SizedBox(width: 8),
            IconButton(
              icon: PhosphorIcon(
                PhosphorIconsRegular.export,
                color: context.colors.textSecondary,
              ),
              onPressed: () {
                final url = ref
                    .read(eventsRepositoryProvider)
                    .exportIcsUrl(channelId, event.id);
                copyToClipboard(context, url);
                ScaffoldMessenger.of(context).showSnackBar(
                  const SnackBar(
                    content: Text('CalDAV / ICS link copied to clipboard'),
                  ),
                );
              },
              tooltip: 'Export CalDAV/ICS Link',
            ),
          ],
        ),

        const SizedBox(height: 20),

        // Attendees section
        Text(
          'ATTENDEES (${event.attendeeCount})',
          style: context.textStyles.labelSmall.copyWith(
            color: context.colors.textTertiary,
            fontWeight: FontWeight.w700,
          ),
        ),
        const SizedBox(height: 8),
        attendeesAsync.when(
          loading: () => const Center(
            child: Padding(
              padding: EdgeInsets.all(8.0),
              child: CircularProgressIndicator(strokeWidth: 2),
            ),
          ),
          error: (err, _) => Text(
            'Could not load attendees',
            style: TextStyle(color: context.colors.textTertiary),
          ),
          data: (attendees) => attendees.isEmpty
              ? Text(
                  'No attendees yet. Be the first to confirm!',
                  style: TextStyle(color: context.colors.textTertiary),
                )
              : Wrap(
                  spacing: 8,
                  runSpacing: 8,
                  children: attendees.map((a) {
                    return Chip(
                      avatar: CircleAvatar(
                        child: Text((a.displayName ?? a.username ?? 'U')[0]
                            .toUpperCase()),
                      ),
                      label: Text(
                        a.displayName ?? a.username ?? 'User',
                        style: TextStyle(color: context.colors.textPrimary),
                      ),
                      backgroundColor: context.colors.backgroundSecondary,
                    );
                  }).toList(),
                ),
        ),

        // Delete / Management options if authorized
        if (canManageEvent) ...[
          const SizedBox(height: 24),
          const Divider(),
          const SizedBox(height: 8),
          TextButton.icon(
            icon: Icon(Icons.delete_outline, color: context.colors.statusRed),
            label: Text(
              'Delete Event',
              style: TextStyle(color: context.colors.statusRed),
            ),
            onPressed: () async {
              final confirm = await showDialog<bool>(
                context: context,
                builder: (ctx) => AlertDialog(
                  title: const Text('Delete Event'),
                  content: const Text(
                      'Are you sure you want to delete this event? This action cannot be undone.'),
                  actions: [
                    TextButton(
                      onPressed: () => Navigator.of(ctx).pop(false),
                      child: const Text('Cancel'),
                    ),
                    TextButton(
                      onPressed: () => Navigator.of(ctx).pop(true),
                      child: Text(
                        'Delete',
                        style: TextStyle(color: context.colors.statusRed),
                      ),
                    ),
                  ],
                ),
              );

              if (confirm == true) {
                await ref
                    .read(channelEventsProvider(channelId).notifier)
                    .deleteEvent(event.id);
                if (context.mounted) {
                  Navigator.of(context).pop();
                }
              }
            },
          ),
        ],
      ],
    );
  }

  String _formatFullDateRange(GuildEvent event) {
    final start = event.startsAt.toLocal();
    final end = event.endsAt.toLocal();
    const months = [
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
    ];
    final dateStr = '${months[start.month - 1]} ${start.day}, ${start.year}';
    final startTime = _timeStr(start);
    final endTime = _timeStr(end);
    return '$dateStr • $startTime – $endTime';
  }

  String _timeStr(DateTime dt) {
    final h = dt.hour % 12 == 0 ? 12 : dt.hour % 12;
    final m = dt.minute.toString().padLeft(2, '0');
    final ampm = dt.hour < 12 ? 'AM' : 'PM';
    return '$h:$m $ampm';
  }

  String _repeatText(EventRepeatType type) {
    switch (type) {
      case EventRepeatType.daily:
        return 'Daily';
      case EventRepeatType.weekly:
        return 'Weekly';
      case EventRepeatType.monthly:
        return 'Monthly';
      case EventRepeatType.never:
        return 'Never';
    }
  }
}

class _DetailRow extends StatelessWidget {
  const _DetailRow({required this.icon, required this.title});

  final IconData icon;
  final String title;

  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        PhosphorIcon(
          icon,
          size: 20,
          color: context.colors.textSecondary,
        ),
        const SizedBox(width: 12),
        Expanded(
          child: Text(
            title,
            style: context.textStyles.heading4.copyWith(
              color: context.colors.textPrimary,
              fontSize: 14,
              fontWeight: FontWeight.w500,
            ),
          ),
        ),
      ],
    );
  }
}
