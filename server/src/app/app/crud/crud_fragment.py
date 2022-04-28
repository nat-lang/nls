from typing import List

from fastapi.encoders import jsonable_encoder
from sqlalchemy.orm import Session

from app.crud.base import CRUDBase
from app.models import Fragment, Example
from app.schemas.fragment import FragmentCreate, FragmentUpdate


def generate_slug(fragment, db):
    slug = "%s-%s" % (fragment.author.last_name, fragment.title)
    uniqifier = ord("a") - 1
    while db.query(Fragment.query.filter(Fragment.slug == slug.exists)).scalar():
        slug = "%s-%s" % (slug, chr(uniqifier))
        uniqifier += 1
    return slug


class CRUDFragment(CRUDBase[Fragment, FragmentCreate, FragmentUpdate]):
    def create(self, db: Session, *, obj_in: FragmentCreate) -> Fragment:
        obj_in_data = jsonable_encoder(obj_in)
        fragment = self.model(**obj_in_data)  # type: ignore
        fragment.slug = generate_slug(fragment, db)
        db.add(fragment)
        db.commit()
        db.refresh(fragment)
        return fragment


fragment = CRUDFragment(Fragment)
