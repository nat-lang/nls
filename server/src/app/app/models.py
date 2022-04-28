from enum import Enum
from typing import TYPE_CHECKING

from sqlalchemy import Boolean, Column, ForeignKey, Integer, String
from sqlalchemy.orm import relationship
from sqlalchemy_utils.types.choice import ChoiceType

from app.db.base_class import Base
from collections import defaultdict, deque

from app.services import JSONSerializableNLTKTree


class Author(Base):
    id = Column(Integer, primary_key=True)
    first_name = Column(String)
    last_name = Column(String)
    fragments = relationship("Fragment")

    @property
    def full_name(self):
        return "%s %s" % (self.first_name, self.last_name)


class ConstituencyParse(Base):
    id = Column(Integer, primary_key=True)
    parse_string = Column(String)
    example_id = Column(Integer, ForeignKey("example.id"), nullable=False)
    example = relationship("Example", back_populates="constituency_parses")

    @property
    def syntax_tree(self):
        return JSONSerializableNLTKTree.fromstring(self.parse_string).json()


class DependencyParse(Base):
    id = Column(Integer, primary_key=True)
    parse_string = Column(String)
    example_id = Column(Integer, ForeignKey("example.id"), nullable=False)
    example = relationship("Example", back_populates="dependency_parses")


class CCGParse(Base):
    id = Column(Integer, primary_key=True)
    parse_string = Column(String)
    example_id = Column(Integer, ForeignKey("example.id"), nullable=False)
    example = relationship("Example", back_populates="ccg_parses")


class Example(Base):
    id = Column(Integer, primary_key=True)
    content = Column(String)
    fragment_id = Column(Integer, ForeignKey("fragment.id"), nullable=False)
    fragment = relationship("Fragment", back_populates="examples")
    description = Column(String)
    label = Column(String)

    constituency_parses = relationship("ConstituencyParse")
    dependency_parses = relationship("DependencyParse")
    ccg_parses = relationship("CCGParse")


class Fragment(Base):
    id = Column(Integer, primary_key=True)
    title = Column(String, index=True)
    author_id = Column(Integer, ForeignKey("author.id"), nullable=False)
    author = relationship("Author", back_populates="fragments")
    content = Column(String)
    examples = relationship("Example")
    slug = Column(String, unique=True)


class User(Base):
    id = Column(Integer, primary_key=True)
    full_name = Column(String, index=True)
    email = Column(String, unique=True, index=True, nullable=False)
    hashed_password = Column(String, nullable=False)
    is_active = Column(Boolean(), default=True)
    is_superuser = Column(Boolean(), default=False)
