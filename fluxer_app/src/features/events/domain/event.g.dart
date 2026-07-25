// GENERATED CODE - DO NOT MODIFY BY HAND
// ignore_for_file: type=lint, unused_element, unnecessary_cast
// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

GuildEvent _$GuildEventFromJson(Map<String, dynamic> json) => GuildEvent(
      id: json['id'] as String,
      channelId: json['channel_id'] as String,
      guildId: json['guild_id'] as String,
      creatorId: json['creator_id'] as String,
      name: json['name'] as String,
      startsAt: DateTime.parse(json['starts_at'] as String),
      endsAt: DateTime.parse(json['ends_at'] as String),
      description: json['description'] as String?,
      locationChannelId: json['location_channel_id'] as String?,
      locationText: json['location_text'] as String?,
      repeatType: json['repeat_type'] != null
          ? EventRepeatType.fromString(json['repeat_type'] as String)
          : EventRepeatType.never,
      repeatInterval: (json['repeat_interval'] as num?)?.toInt() ?? 1,
      attendeeCount: (json['attendee_count'] as num?)?.toInt() ?? 0,
      isAttending: json['is_attending'] as bool? ?? false,
      createdAt: json['created_at'] != null
          ? DateTime.parse(json['created_at'] as String)
          : null,
      updatedAt: json['updated_at'] != null
          ? DateTime.parse(json['updated_at'] as String)
          : null,
    );

Map<String, dynamic> _$GuildEventToJson(GuildEvent instance) =>
    <String, dynamic>{
      'id': instance.id,
      'channel_id': instance.channelId,
      'guild_id': instance.guildId,
      'creator_id': instance.creatorId,
      'name': instance.name,
      'starts_at': instance.startsAt.toIso8601String(),
      'ends_at': instance.endsAt.toIso8601String(),
      'description': instance.description,
      'location_channel_id': instance.locationChannelId,
      'location_text': instance.locationText,
      'repeat_type': instance.repeatType.value,
      'repeat_interval': instance.repeatInterval,
      'attendee_count': instance.attendeeCount,
      'is_attending': instance.isAttending,
      'created_at': instance.createdAt?.toIso8601String(),
      'updated_at': instance.updatedAt?.toIso8601String(),
    };

EventAttendee _$EventAttendeeFromJson(Map<String, dynamic> json) =>
    EventAttendee(
      userId: json['user_id'] as String,
      rsvpAt: DateTime.parse(json['rsvp_at'] as String),
      username: json['username'] as String?,
      avatarHash: json['avatar_hash'] as String?,
      displayName: json['display_name'] as String?,
    );

Map<String, dynamic> _$EventAttendeeToJson(EventAttendee instance) =>
    <String, dynamic>{
      'user_id': instance.userId,
      'rsvp_at': instance.rsvpAt.toIso8601String(),
      'username': instance.username,
      'avatar_hash': instance.avatarHash,
      'display_name': instance.displayName,
    };
